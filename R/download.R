glc_validate_resource_names <- function(x, resources) {
  declared <- unique(glc_resources(x)$resource)
  if (is.null(resources)) return(NULL)
  if (!is.character(resources) || anyNA(resources) || any(!nzchar(resources))) {
    glc_abort("{.arg resources} must contain non-empty resource names.")
  }
  unknown <- setdiff(resources, declared)
  if (length(unknown) > 0L) {
    glc_abort(
      "Unknown resource name{?s}: {.val {unknown}}. Declared resources are {.val {declared}}."
    )
  }
  unique(resources)
}

glc_expand_resource_names <- function(x, names) {
  paths <- character()
  for (name in names) {
    resource <- glc_resource_descriptor(x, name)
    paths <- c(paths, glc_resource_file_paths(x, resource))
  }
  unique(paths)
}

glc_schema_dependency_paths <- function(x) {
  resources <- x$descriptor$resources %||% list()
  candidates <- glc_unique_chr(
    x$descriptor$profile,
    lapply(resources, function(resource) {
      list(resource$profile, resource$schema, resource$jsonSchema)
    })
  )
  candidates <- candidates[
    !grepl("^https?://", candidates) & grepl("/|\\.json$", candidates)
  ]
  paths <- character()
  for (candidate in candidates) {
    resolved <- tryCatch(
      glc_expand_declared_path(x, candidate),
      glcdp_missing_path = function(cnd) character()
    )
    paths <- c(paths, resolved)
  }
  unique(paths)
}

glc_filter_download_files <- function(file_table, files) {
  if (is.null(files)) return(file_table)
  if (!is.character(files) || anyNA(files) || any(!nzchar(files))) {
    glc_abort("{.arg files} must contain non-empty paths or basenames.")
  }
  keep <- file_table$path %in%
    files |
    file_table$declared_path %in% files |
    basename(file_table$path) %in% files
  unmatched <- files[
    !files %in%
      c(
        file_table$path,
        file_table$declared_path,
        basename(file_table$path)
      )
  ]
  if (length(unmatched) > 0L) {
    glc_abort("Selected data file{?s} not found: {.path {unmatched}}.")
  }
  file_table[keep, , drop = FALSE]
}

glc_download_selection <- function(
  x,
  include,
  dataset_id,
  file_group,
  resources,
  files
) {
  inventory <- glc_resources(x)
  core <- unique(inventory$resource[inventory$core])
  additional <- unique(inventory$resource[!inventory$core])
  resources <- glc_validate_resource_names(x, resources)

  metadata_names <- if (!is.null(resources)) resources else core
  metadata_paths <- glc_expand_resource_names(x, metadata_names)
  schema_paths <- glc_schema_dependency_paths(x)
  data_paths <- character()

  if (include %in% c("data", "all")) {
    file_table <- glc_files(
      x,
      dataset_id = dataset_id,
      file_group = file_group
    )
    file_table <- glc_filter_download_files(file_table, files)
    if (nrow(file_table) == 0L && length(additional) == 0L) {
      glc_abort("The selected package subset contains no data files.")
    }
    data_paths <- file_table$path

    has_data_selector <- !is.null(dataset_id) ||
      !is.null(file_group) ||
      !is.null(files)
    if (!has_data_selector && length(additional) > 0L) {
      selected_additional <- if (!is.null(resources)) {
        intersect(resources, additional)
      } else {
        additional
      }
      data_paths <- c(
        data_paths,
        glc_expand_resource_names(x, selected_additional)
      )
    }
  }

  if (identical(include, "all")) {
    all_names <- if (is.null(resources)) unique(inventory$resource) else
      resources
    data_paths <- c(data_paths, glc_expand_resource_names(x, all_names))
  }

  if (identical(include, "metadata")) {
    selected <- c("datapackage.json", metadata_paths, schema_paths)
  } else {
    # Data subsets carry the complete core metadata closure needed to reopen and
    # interpret them.
    core_paths <- glc_expand_resource_names(x, core)
    selected <- c("datapackage.json", core_paths, schema_paths, data_paths)
  }
  unique(vapply(selected, glc_safe_path, character(1)))
}

glc_write_manifest <- function(x, destination, include, selection, results) {
  manifest <- list(
    manifest_version = "1.0",
    generated_at_utc = format(
      Sys.time(),
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ),
    repository = if (is.na(x$repo)) NULL else x$repo,
    commit = if (is.na(x$commit)) NULL else x$commit,
    registry_verified = if (is.na(x$verified)) NULL else x$verified,
    source_ref = x$ref_kind,
    schema_version = x$schema_version,
    selection = list(include = include, paths = as.list(selection)),
    files = lapply(seq_len(nrow(results)), function(i) {
      list(
        path = results$path[[i]],
        storage = results$storage[[i]],
        bytes = results$bytes[[i]],
        sha256 = results$sha256[[i]],
        lfs_oid = if (is.na(results$lfs_oid[[i]])) NULL else
          results$lfs_oid[[i]]
      )
    })
  )
  path <- file.path(destination, "glcdp-manifest.json")
  temporary <- tempfile(".glcdp-manifest-", tmpdir = destination)
  on.exit(unlink(temporary), add = TRUE)
  jsonlite::write_json(
    manifest,
    temporary,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null"
  )
  if (file.exists(path)) unlink(path)
  if (!file.rename(temporary, path)) {
    glc_abort("Could not finalize {.path {path}}.")
  }
  invisible(path)
}

#' Download package metadata or data
#'
#' Downloads only descriptor-declared and dataset-referenced content, preserves
#' repository-relative paths, and writes a reproducibility manifest.
#'
#' @param x A package opened with [glc_open()].
#' @param dest_dir Destination directory.
#' @param include One of `"metadata"`, `"data"`, or `"all"`. Metadata is the
#'   safe default.
#' @param dataset_id Optional dataset id selection for data downloads.
#' @param file_group Optional file-group selection.
#' @param resources Optional descriptor resource names.
#' @param files Optional exact paths, declared paths, or basenames.
#' @param overwrite Whether existing files may be replaced.
#'
#' @return A tibble recording downloaded paths, storage, size, and hashes.
#' @export
glc_download <- function(
  x,
  dest_dir,
  include = c("metadata", "data", "all"),
  dataset_id = NULL,
  file_group = NULL,
  resources = NULL,
  files = NULL,
  overwrite = FALSE
) {
  glc_assert_package(x)
  glc_assert_string(dest_dir, "dest_dir")
  include <- match.arg(include)
  glc_assert_flag(overwrite, "overwrite")
  selection <- glc_download_selection(
    x,
    include,
    dataset_id,
    file_group,
    resources,
    files
  )

  destination <- normalizePath(dest_dir, winslash = "/", mustWork = FALSE)
  if (x$source_type == "local") {
    source_root <- paste0(normalizePath(x$root, winslash = "/"), "/")
    if (
      identical(destination, sub("/$", "", source_root)) ||
        startsWith(paste0(destination, "/"), source_root)
    ) {
      glc_abort("{.arg dest_dir} must not be inside the local source package.")
    }
  }
  dir.create(destination, recursive = TRUE, showWarnings = FALSE)

  target_paths <- file.path(destination, selection)
  manifest_path <- file.path(destination, "glcdp-manifest.json")
  existing <- c(
    target_paths[file.exists(target_paths)],
    manifest_path[file.exists(manifest_path)]
  )
  if (length(existing) > 0L && !overwrite) {
    glc_abort(
      "Destination already contains file{?s}: {.path {existing}}. Set `overwrite = TRUE` to replace them.",
      class = "glcdp_file_exists"
    )
  }

  rows <- lapply(seq_along(selection), function(i) {
    source_path <- selection[[i]]
    result <- glc_atomic_transfer(
      x,
      source_path,
      target_paths[[i]],
      overwrite = overwrite
    )
    tibble::tibble(
      path = result$path,
      destination = target_paths[[i]],
      storage = result$storage,
      bytes = result$bytes,
      sha256 = result$sha256,
      lfs_oid = result$lfs_oid
    )
  })
  results <- dplyr::bind_rows(rows)
  glc_write_manifest(x, destination, include, selection, results)
  results
}
