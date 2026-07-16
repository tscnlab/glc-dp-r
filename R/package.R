new_glc_package <- function(
  source_type,
  root = NULL,
  repo = NA_character_,
  commit = NA_character_,
  ref_kind = "local",
  verified = NA,
  descriptor,
  schema_version = NA_character_,
  registry_row = NULL,
  manifest = NULL,
  token = NULL,
  cache_dir = NULL
) {
  transport <- new.env(parent = emptyenv())
  transport$token <- token
  transport$cache_dir <- cache_dir
  transport$tree <- NULL
  transport$local_paths <- NULL
  transport$model <- NULL
  transport$resource_raw <- list()

  structure(
    list(
      source_type = source_type,
      root = root,
      repo = repo,
      commit = commit,
      ref_kind = ref_kind,
      verified = verified,
      descriptor = descriptor,
      schema_version = schema_version,
      registry_row = registry_row,
      manifest = manifest,
      transport = transport
    ),
    class = "glc_package"
  )
}

glc_registry_argument <- function(registry) {
  if (is.null(registry)) {
    return(glc_packages())
  }
  if (inherits(registry, "glc_registry")) {
    return(registry)
  }
  if (is.character(registry) && length(registry) == 1L) {
    return(glc_packages(registry = registry))
  }
  glc_abort(
    "{.arg registry} must be `NULL`, a registry source, or a value returned by {.fn glc_packages}."
  )
}

glc_parse_github_repo <- function(source) {
  if (grepl("^https?://github\\.com/", source, ignore.case = TRUE)) {
    value <- sub("^https?://github\\.com/", "", source, ignore.case = TRUE)
    value <- sub("[?#].*$", "", value)
    parts <- strsplit(value, "/", fixed = TRUE)[[1L]]
    if (length(parts) >= 2L) {
      return(paste(parts[[1L]], sub("\\.git$", "", parts[[2L]]), sep = "/"))
    }
  }
  if (grepl("^[^/]+/[^/]+$", source)) {
    return(sub("\\.git$", "", source))
  }
  NA_character_
}

glc_match_registry <- function(source, packages) {
  parsed_repo <- glc_parse_github_repo(source)
  if (!is.na(parsed_repo)) {
    index <- which(tolower(packages$repository) == tolower(parsed_repo))
  } else {
    index <- which(tolower(packages$id) == tolower(source))
  }
  if (length(index) > 1L) {
    glc_abort("Registry source {.val {source}} matches more than one package.")
  }
  if (length(index) == 0L) NULL else packages[index, , drop = FALSE]
}

glc_local_manifest <- function(root) {
  path <- file.path(root, "glcdp-manifest.json")
  if (file.exists(path)) glc_read_json_file(path, simplify = FALSE) else NULL
}

glc_detect_schema_version <- function(x) {
  version <- glc_scalar_character(x$descriptor$schema_version)
  if (!is.na(version)) {
    return(version)
  }

  profile <- glc_scalar_character(x$descriptor$profile)
  if (!is.na(profile)) {
    profile_match <- regmatches(
      profile,
      regexpr("[123]\\.0\\.0", profile, perl = TRUE)
    )
    if (length(profile_match) == 1L && nzchar(profile_match)) {
      return(profile_match)
    }
  }

  dataset_value <- tryCatch(
    glc_read_named_resource_raw(x, "datasets"),
    error = function(cnd) NULL
  )
  records <- glc_records(dataset_value)
  record_versions <- unique(vapply(
    records,
    function(record) glc_scalar_character(record$schema_version),
    character(1)
  ))
  record_versions <- record_versions[!is.na(record_versions)]
  if (length(record_versions) == 1L) {
    return(record_versions)
  }

  resource_names <- vapply(
    x$descriptor$resources %||% list(),
    function(resource) glc_scalar_character(resource$name),
    character(1)
  )
  required <- c(
    "study",
    "participants",
    "datasets",
    "devices",
    "device_datasheets"
  )
  if (all(required %in% resource_names)) {
    glc_warn(
      "The package does not declare `schema_version`; treating its recognizable legacy structure as schema 1.0.0.",
      class = "glcdp_inferred_schema"
    )
    return("1.0.0")
  }

  glc_abort(
    "The data package does not declare a recognizable GLC schema version.",
    class = "glcdp_schema_error"
  )
}

glc_check_schema_version <- function(version, quiet = FALSE) {
  supported <- c("1.0.0", "2.0.0", "3.0.0")
  if (!version %in% supported) {
    glc_abort(
      "Unsupported GLC schema version {.val {version}}. Supported versions are {.val {supported}}.",
      class = "glcdp_unsupported_schema"
    )
  }
  if (
    identical(version, "3.0.0") &&
      !isTRUE(.glcdp_state$schema_3_notice) &&
      !quiet
  ) {
    glc_inform(
      "Schema 3.0.0 support follows the current development schema and is experimental."
    )
    .glcdp_state$schema_3_notice <- TRUE
  }
  invisible(version)
}

glc_open_local <- function(source, token, cache_dir, quiet) {
  if (
    file.exists(source) &&
      !dir.exists(source) &&
      basename(source) != "datapackage.json"
  ) {
    glc_abort(
      "Local file {.path {source}} is not `datapackage.json`; supply the package directory or its descriptor."
    )
  }
  root <- if (dir.exists(source)) source else dirname(source)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  descriptor_path <- file.path(root, "datapackage.json")
  if (!file.exists(descriptor_path)) {
    glc_abort(
      "Local directory {.path {root}} does not contain `datapackage.json`.",
      class = "glcdp_missing_descriptor"
    )
  }
  manifest <- glc_local_manifest(root)
  descriptor <- glc_read_json_file(descriptor_path, simplify = FALSE)
  package <- new_glc_package(
    source_type = "local",
    root = root,
    repo = glc_scalar_character(manifest$repository),
    commit = glc_scalar_character(manifest$commit),
    ref_kind = "local",
    verified = glc_scalar_logical(manifest$registry_verified),
    descriptor = descriptor,
    manifest = manifest,
    token = token,
    cache_dir = cache_dir
  )
  package$schema_version <- glc_detect_schema_version(package)
  glc_check_schema_version(package$schema_version, quiet = quiet)
  package
}

glc_open_remote <- function(source, ref, token, cache_dir, registry, quiet) {
  packages <- glc_registry_argument(registry)
  registry_row <- glc_match_registry(source, packages)
  repo <- glc_parse_github_repo(source)
  if (is.na(repo) && !is.null(registry_row)) {
    repo <- registry_row$repository[[1L]]
  }
  if (is.na(repo)) {
    glc_abort(
      "Could not resolve {.val {source}} to a registered package or `owner/repository` GitHub source."
    )
  }

  if (identical(ref, "latest_pass")) {
    if (is.null(registry_row) || !isTRUE(registry_row$has_latest_pass[[1L]])) {
      glc_abort(
        "No passing revision is recorded for {.val {repo}}. Use `ref = \"current\"` or an exact 40-character commit SHA to inspect an unverified revision.",
        class = "glcdp_no_passing_revision"
      )
    }
    commit <- registry_row$latest_pass_commit[[1L]]
    ref_kind <- "latest_pass"
    if (
      !is.na(registry_row$current_commit[[1L]]) &&
        !identical(commit, registry_row$current_commit[[1L]]) &&
        !quiet
    ) {
      glc_inform(c(
        "Using the latest passing revision of {.val {repo}}.",
        "i" = "The current revision is {.val {substr(registry_row$current_commit[[1L]], 1L, 12L)}} with status {.val {registry_row$current_status[[1L]]}}; selected {.val {substr(commit, 1L, 12L)}}."
      ))
    }
  } else if (identical(ref, "current")) {
    if (is.null(registry_row) || is.na(registry_row$current_commit[[1L]])) {
      glc_abort("No current registry revision is available for {.val {repo}}.")
    }
    commit <- registry_row$current_commit[[1L]]
    ref_kind <- "current"
    if (!isTRUE(registry_row$is_current_pass[[1L]])) {
      glc_warn(
        "Opening current revision {.val {substr(commit, 1L, 12L)}} of {.val {repo}}, whose registry status is {.val {registry_row$current_status[[1L]]}}.",
        class = "glcdp_nonpassing_revision"
      )
    }
  } else {
    if (!grepl("^[0-9a-fA-F]{40}$", ref)) {
      glc_abort(
        "{.arg ref} must be `\"latest_pass\"`, `\"current\"`, or an exact 40-character commit SHA."
      )
    }
    commit <- tolower(ref)
    ref_kind <- "commit"
  }

  verified <- FALSE
  if (!is.null(registry_row)) {
    verified <- commit %in%
      c(
        registry_row$current_commit[[1L]],
        registry_row$latest_pass_commit[[1L]]
      )
  }
  package <- new_glc_package(
    source_type = "remote",
    repo = repo,
    commit = commit,
    ref_kind = ref_kind,
    verified = verified,
    descriptor = list(),
    registry_row = registry_row,
    token = token,
    cache_dir = cache_dir
  )
  raw <- glc_fetch_remote_raw(package, "datapackage.json")
  package$descriptor <- glc_read_json_text(rawToChar(raw), "datapackage.json")
  package$schema_version <- glc_detect_schema_version(package)
  glc_check_schema_version(package$schema_version, quiet = quiet)
  package
}

#' Open a Global Light Commons data package
#'
#' Opens a local package or resolves a GitHub-hosted package at an immutable
#' commit. Registered packages default to their latest passing revision.
#'
#' @param source Registry id, `owner/repository`, GitHub URL, local package
#'   directory, or local `datapackage.json` path.
#' @param ref Remote revision: `"latest_pass"`, `"current"`, or an exact
#'   40-character commit SHA.
#' @param token Optional GitHub token. When omitted, `GITHUB_PAT` and then
#'   `GITHUB_TOKEN` are consulted.
#' @param cache_dir Optional explicit persistent cache directory. The default
#'   uses session-temporary storage for remote reads.
#' @param registry Optional registry object, URL, or local JSON path.
#' @param quiet Suppress informational messages. Warnings remain visible.
#'
#' @return A `glc_package` handle.
#' @export
#'
#' @examplesIf interactive()
#' package <- glc_open("tscnlab/guidolin-glee-datasetv2")
#' package
glc_open <- function(
  source,
  ref = "latest_pass",
  token = NULL,
  cache_dir = NULL,
  registry = NULL,
  quiet = FALSE
) {
  glc_assert_string(source, "source")
  glc_assert_string(ref, "ref")
  glc_assert_flag(quiet, "quiet")
  token <- glc_token(token)
  if (!is.null(cache_dir)) {
    glc_assert_string(cache_dir, "cache_dir")
    cache_dir <- normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
  }

  is_local <- dir.exists(source) || file.exists(source)
  if (is_local) {
    return(glc_open_local(source, token, cache_dir, quiet))
  }
  glc_open_remote(source, ref, token, cache_dir, registry, quiet)
}

#' Report supported GLC schema versions
#'
#' @return A tibble describing support status for each schema version.
#' @export
#'
#' @examples
#' glc_schema_versions()
glc_schema_versions <- function() {
  tibble::tibble(
    version = c("1.0.0", "2.0.0", "3.0.0"),
    status = c("stable", "stable", "experimental"),
    notes = c(
      "Legacy packages may omit the root schema version.",
      "Current released schema supported by the validator.",
      "Follows the schema-3.0.0-development branch."
    )
  )
}

#' @export
print.glc_package <- function(x, ...) {
  location <- if (x$source_type == "local") {
    x$root
  } else {
    paste0(x$repo, "@", substr(x$commit, 1L, 12L))
  }
  cat("<GLC data package>\n")
  cat("Source: ", location, "\n", sep = "")
  cat("Schema: ", x$schema_version, "\n", sep = "")
  if (!is.na(x$verified)) {
    cat(
      "Registry revision: ",
      if (isTRUE(x$verified)) "verified" else "unverified",
      "\n",
      sep = ""
    )
  }
  invisible(x)
}
