`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

.glcdp_state <- new.env(parent = emptyenv())
.glcdp_state$registry <- list()
.glcdp_state$schema_3_notice <- FALSE

glc_abort <- function(message, ..., class = NULL, .envir = parent.frame()) {
  cli::cli_abort(
    message,
    ...,
    class = class,
    call = NULL,
    .envir = .envir
  )
}

glc_warn <- function(message, ..., class = NULL, .envir = parent.frame()) {
  cli::cli_warn(
    message,
    ...,
    class = class,
    call = NULL,
    .envir = .envir
  )
}

glc_inform <- function(message, ..., class = NULL, .envir = parent.frame()) {
  cli::cli_inform(message, ..., class = class, .envir = .envir)
}

glc_assert_string <- function(x, arg, allow_empty = FALSE) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    glc_abort("{.arg {arg}} must be one non-missing string.")
  }
  if (!allow_empty && !nzchar(x)) {
    glc_abort("{.arg {arg}} must not be empty.")
  }
  invisible(x)
}

glc_assert_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    glc_abort("{.arg {arg}} must be `TRUE` or `FALSE`.")
  }
  invisible(x)
}

glc_assert_package <- function(x) {
  if (!inherits(x, "glc_package")) {
    glc_abort(
      "{.arg x} must be a GLC package opened with {.fn glc_open}; supplied class: {.cls {class(x)[[1L]] %||% 'NULL'}}."
    )
  }
  invisible(x)
}

glc_scalar_character <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) {
    return(default)
  }
  as.character(x[[1L]])
}

glc_scalar_logical <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0L || is.na(x[[1L]])) {
    return(default)
  }
  isTRUE(x[[1L]])
}

glc_scalar_number <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }
  value <- suppressWarnings(as.numeric(x[[1L]]))
  if (length(value) == 0L || is.na(value)) default else value
}

glc_count_value <- function(x) {
  if (is.null(x)) {
    return(0L)
  }
  if (is.atomic(x) && length(x) == 1L && !is.na(x)) {
    value <- suppressWarnings(as.integer(x))
    if (!is.na(value)) {
      return(value)
    }
  }
  as.integer(length(x))
}

glc_compact_character <- function(x) {
  x <- as.character(unlist(x, recursive = TRUE, use.names = FALSE))
  unique(x[!is.na(x) & nzchar(x)])
}

glc_records <- function(x) {
  if (is.null(x)) {
    return(list())
  }
  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) as.list(x[i, , drop = FALSE])))
  }
  if (!is.list(x)) {
    return(list(x))
  }
  if (length(x) == 0L) {
    return(list())
  }
  if (is.null(names(x)) || all(!nzchar(names(x)))) {
    return(x)
  }
  list(x)
}

glc_is_url <- function(x) {
  is.character(x) && length(x) == 1L && grepl("^https?://", x)
}

glc_token <- function(token = NULL) {
  if (!is.null(token)) {
    glc_assert_string(token, "token")
    return(token)
  }
  value <- Sys.getenv("GITHUB_PAT", unset = "")
  if (!nzchar(value)) {
    value <- Sys.getenv("GITHUB_TOKEN", unset = "")
  }
  if (nzchar(value)) value else NULL
}

glc_safe_path <- function(path, arg = "path") {
  glc_assert_string(path, arg)
  path <- gsub("\\\\", "/", path)
  path <- sub("^\\./", "", path)
  components <- strsplit(path, "/", fixed = TRUE)[[1L]]
  if (
    startsWith(path, "/") ||
      grepl("^[A-Za-z]:", path) ||
      any(components %in% c("", ".."))
  ) {
    glc_abort(
      "Unsafe package path in {.arg {arg}}: {.path {path}}. Paths must be relative and must not contain parent traversal."
    )
  }
  paste(components[components != "."], collapse = "/")
}

glc_path_encode <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1L]]
  paste(
    vapply(parts, utils::URLencode, character(1), reserved = TRUE),
    collapse = "/"
  )
}

glc_read_json_text <- function(text, source) {
  tryCatch(
    jsonlite::fromJSON(text, simplifyVector = FALSE),
    error = function(cnd) {
      glc_abort(
        "Could not parse JSON from {.path {source}}: {conditionMessage(cnd)}",
        class = "glcdp_json_error"
      )
    }
  )
}

glc_read_json_file <- function(path, simplify = FALSE) {
  if (!file.exists(path)) {
    glc_abort("JSON file does not exist: {.path {path}}.")
  }
  tryCatch(
    jsonlite::fromJSON(path, simplifyVector = simplify),
    error = function(cnd) {
      glc_abort(
        "Could not parse JSON from {.path {path}}: {conditionMessage(cnd)}",
        class = "glcdp_json_error"
      )
    }
  )
}

glc_empty_chr <- function() character()

glc_unique_chr <- function(...) {
  values <- unlist(list(...), recursive = TRUE, use.names = FALSE)
  values <- as.character(values)
  unique(values[!is.na(values) & nzchar(values)])
}
