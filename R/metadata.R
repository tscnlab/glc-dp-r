glc_default_metadata_resources <- function(x) {
  declared <- glc_resources(x)$resource
  intersect(glc_core_resource_names(), unique(declared))
}

glc_resource_file_paths <- function(x, resource) {
  paths <- glc_compact_character(resource$path)
  if (length(paths) == 0L) {
    glc_abort(
      "Resource {.val {glc_scalar_character(resource$name)}} does not declare a path."
    )
  }
  unique(unlist(
    lapply(paths, function(path) glc_expand_declared_path(x, path)),
    use.names = FALSE
  ))
}

glc_resource_format <- function(resource, path) {
  format <- tolower(glc_scalar_character(resource$format))
  if (!is.na(format) && nzchar(format)) {
    return(format)
  }
  extension <- tolower(tools::file_ext(path))
  if (nzchar(extension)) extension else NA_character_
}

glc_read_tabular_resource <- function(path, resource) {
  dialect <- resource$dialect %||% list()
  delimiter <- glc_scalar_character(dialect$delimiter)
  if (is.na(delimiter)) {
    extension <- tolower(tools::file_ext(path))
    delimiter <- if (extension == "tsv") "\t" else ","
  }
  decimal_mark <- glc_scalar_character(
    dialect$decimalChar %||% dialect$decimal_mark,
    "."
  )
  encoding <- glc_scalar_character(resource$encoding, "UTF-8")
  readr::read_delim(
    path,
    delim = delimiter,
    locale = readr::locale(decimal_mark = decimal_mark, encoding = encoding),
    show_col_types = FALSE,
    progress = FALSE,
    name_repair = "minimal"
  )
}

glc_read_resource_file <- function(x, path, resource) {
  local_path <- glc_materialize_file(x, path)
  format <- glc_resource_format(resource, path)
  media_type <- tolower(glc_scalar_character(
    resource$mediatype %||% resource$mediaType,
    ""
  ))
  if (identical(format, "json") || grepl("json", media_type, fixed = TRUE)) {
    value <- glc_read_json_file(local_path, simplify = TRUE)
    if (is.data.frame(value)) tibble::as_tibble(value) else value
  } else if (
    format %in% c("csv", "tsv", "txt") || grepl("csv", media_type, fixed = TRUE)
  ) {
    glc_read_tabular_resource(local_path, resource)
  } else {
    glc_abort(
      "Resource file {.path {path}} has unsupported metadata format {.val {format}}.",
      class = "glcdp_unsupported_metadata_format"
    )
  }
}

#' Load metadata resources
#'
#' Loads core metadata by default. Additional descriptor resources can be
#' requested explicitly by name.
#'
#' @param x A package opened with [glc_open()].
#' @param resources Optional resource names. The default selects declared core
#'   metadata resources.
#'
#' @return A named list with one element per requested resource. Tabular
#'   resources are returned as tibbles; directory resources contain named
#'   sub-lists.
#' @export
glc_metadata <- function(x, resources = NULL) {
  glc_assert_package(x)
  if (is.null(resources)) {
    resources <- glc_default_metadata_resources(x)
  }
  if (!is.character(resources) || anyNA(resources) || any(!nzchar(resources))) {
    glc_abort("{.arg resources} must contain non-empty resource names.")
  }
  declared <- unique(glc_resources(x)$resource)
  unknown <- setdiff(resources, declared)
  if (length(unknown) > 0L) {
    glc_abort(
      "Unknown resource name{?s}: {.val {unknown}}. Declared resources are {.val {declared}}.",
      class = "glcdp_missing_resource"
    )
  }

  result <- lapply(resources, function(name) {
    resource <- glc_resource_descriptor(x, name)
    paths <- glc_resource_file_paths(x, resource)
    values <- lapply(paths, function(path) {
      glc_read_resource_file(x, path, resource)
    })
    if (
      length(values) == 1L &&
        !any(endsWith(glc_compact_character(resource$path), "/"))
    ) {
      values[[1L]]
    } else {
      stats::setNames(values, paths)
    }
  })
  stats::setNames(result, resources)
}

glc_metadata_leaf_table <- function(metadata) {
  rows <- list()
  row_index <- 0L

  add_leaf <- function(resource, record, field, value) {
    if (length(value) == 0L) return(invisible(NULL))
    values <- as.character(value)
    for (item in values) {
      row_index <<- row_index + 1L
      rows[[row_index]] <<- tibble::tibble(
        resource = resource,
        record = as.integer(record),
        field = field,
        value = item,
        context = if (is.na(record)) "object" else paste("record", record)
      )
    }
    invisible(NULL)
  }

  walk <- function(value, resource, field = "", record = NA_integer_) {
    if (is.null(value)) return(invisible(NULL))
    if (is.data.frame(value)) {
      if (nrow(value) == 0L) return(invisible(NULL))
      for (i in seq_len(nrow(value))) {
        child_record <- if (is.na(record)) i else record
        for (name in names(value)) {
          column <- value[[name]]
          child <- if (is.data.frame(column)) {
            column[i, , drop = FALSE]
          } else if (is.list(column)) {
            column[[i]]
          } else {
            column[i]
          }
          child_field <- if (nzchar(field)) paste(field, name, sep = ".") else
            name
          walk(child, resource, child_field, child_record)
        }
      }
      return(invisible(NULL))
    }
    if (is.list(value)) {
      child_names <- names(value)
      if (is.null(child_names) || all(!nzchar(child_names))) {
        for (i in seq_along(value)) {
          child_field <- paste0(field, "[", i, "]")
          child_record <- if (is.na(record)) i else record
          walk(value[[i]], resource, child_field, child_record)
        }
      } else {
        for (i in seq_along(value)) {
          name <- child_names[[i]]
          child_field <- if (nzchar(field)) paste(field, name, sep = ".") else
            name
          walk(value[[i]], resource, child_field, record)
        }
      }
      return(invisible(NULL))
    }
    if (is.atomic(value)) {
      add_leaf(resource, record, field, value)
    }
    invisible(NULL)
  }

  for (resource in names(metadata)) {
    walk(metadata[[resource]], resource)
  }
  if (length(rows) == 0L) {
    return(tibble::tibble(
      resource = character(),
      record = integer(),
      field = character(),
      value = character(),
      context = character()
    ))
  }
  dplyr::bind_rows(rows)
}

#' Search metadata values or field paths
#'
#' @param x A package opened with [glc_open()].
#' @param query Text or regular expression to search for.
#' @param resources Optional metadata resource names.
#' @param fields Optional exact field names or complete field paths to include.
#' @param fixed Treat `query` as fixed text rather than a regular expression.
#' @param ignore_case Ignore letter case while matching.
#' @param search_in Where to match `query`: scalar metadata `"values"`, complete
#'   field paths `"fields"`, or `"both"`. The default is `"values"`.
#'
#' @details
#' Field searches return the same leaf-level rows as value searches. A field
#' path that contains multiple scalar values therefore produces one row per
#' value. The `fields` argument can be combined with any `search_in` mode to
#' restrict which field paths are searched.
#'
#' @return A tibble of matching scalar metadata values and their field paths.
#' @export
glc_search_metadata <- function(
  x,
  query,
  resources = NULL,
  fields = NULL,
  fixed = TRUE,
  ignore_case = TRUE,
  search_in = c("values", "fields", "both")
) {
  glc_assert_package(x)
  glc_assert_string(query, "query", allow_empty = TRUE)
  glc_assert_flag(fixed, "fixed")
  glc_assert_flag(ignore_case, "ignore_case")
  search_in <- match.arg(search_in)
  if (!is.null(fields) && (!is.character(fields) || anyNA(fields))) {
    glc_abort(
      "{.arg fields} must be a character vector without missing values."
    )
  }
  leaves <- glc_metadata_leaf_table(glc_metadata(x, resources = resources))
  if (!is.null(fields)) {
    field_match <- vapply(
      leaves$field,
      function(path) {
        any(path == fields | endsWith(path, paste0(".", fields)))
      },
      logical(1)
    )
    leaves <- leaves[field_match, , drop = FALSE]
  }

  match_text <- function(text) {
    search_query <- query
    search_text <- text
    search_ignore_case <- ignore_case
    if (fixed && ignore_case) {
      search_query <- tolower(search_query)
      search_text <- tolower(search_text)
      search_ignore_case <- FALSE
    }
    tryCatch(
      grepl(
        search_query,
        search_text,
        fixed = fixed,
        ignore.case = search_ignore_case
      ),
      error = function(cnd) {
        glc_abort("Invalid metadata search pattern: {conditionMessage(cnd)}.")
      }
    )
  }
  keep <- switch(
    search_in,
    values = match_text(leaves$value),
    fields = match_text(leaves$field),
    both = match_text(leaves$value) | match_text(leaves$field)
  )
  keep[is.na(keep)] <- FALSE
  leaves[keep, , drop = FALSE]
}
