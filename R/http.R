glc_request <- function(
  url,
  token = NULL,
  accept = "application/vnd.github+json",
  error_status = TRUE
) {
  request <- httr2::request(url) |>
    httr2::req_user_agent("glcdp/0.0.0.9000") |>
    httr2::req_headers(Accept = accept) |>
    httr2::req_retry(
      max_tries = 4L,
      is_transient = function(response) {
        status <- httr2::resp_status(response)
        status == 429L || status >= 500L
      }
    )

  if (!is.null(token)) {
    request <- httr2::req_headers(
      request,
      Authorization = paste("Bearer", token)
    )
  }
  if (!error_status) {
    request <- httr2::req_error(request, is_error = function(response) FALSE)
  }
  request
}

glc_perform <- function(request, context, path = NULL, ok = c(200L, 206L)) {
  response <- tryCatch(
    httr2::req_perform(request, path = path),
    error = function(cnd) {
      glc_abort(
        "{context} failed because the remote service could not be reached: {conditionMessage(cnd)}",
        class = "glcdp_http_error"
      )
    }
  )
  status <- httr2::resp_status(response)
  if (!status %in% ok) {
    glc_abort(
      "{context} failed with HTTP status {status}.",
      class = c("glcdp_http_status", paste0("glcdp_http_", status))
    )
  }
  response
}

glc_get_raw <- function(
  url,
  token = NULL,
  accept = "application/vnd.github.raw+json"
) {
  request <- glc_request(
    url,
    token = token,
    accept = accept,
    error_status = FALSE
  )
  response <- glc_perform(request, context = "GitHub download")
  httr2::resp_body_raw(response)
}

glc_get_json <- function(url, token = NULL) {
  request <- glc_request(url, token = token, error_status = FALSE)
  response <- glc_perform(request, context = "GitHub API request")
  tryCatch(
    httr2::resp_body_json(response, simplifyVector = FALSE),
    error = function(cnd) {
      glc_abort(
        "GitHub returned malformed JSON: {conditionMessage(cnd)}",
        class = "glcdp_json_error"
      )
    }
  )
}

glc_contents_url <- function(x, path) {
  path <- glc_safe_path(path)
  paste0(
    "https://api.github.com/repos/",
    x$repo,
    "/contents/",
    glc_path_encode(path),
    "?ref=",
    utils::URLencode(x$commit, reserved = TRUE)
  )
}

glc_raw_contents_url <- function(x, path) {
  path <- glc_safe_path(path)
  paste0(
    "https://raw.githubusercontent.com/",
    x$repo,
    "/",
    utils::URLencode(x$commit, reserved = TRUE),
    "/",
    glc_path_encode(path)
  )
}

glc_remote_file_request <- function(x, path, error_status = TRUE) {
  use_raw_host <- is.null(x$transport$token)
  url <- if (use_raw_host) {
    glc_raw_contents_url(x, path)
  } else {
    glc_contents_url(x, path)
  }
  accept <- if (use_raw_host) {
    "application/octet-stream"
  } else {
    "application/vnd.github.raw+json"
  }
  glc_request(
    url,
    token = x$transport$token,
    accept = accept,
    error_status = error_status
  )
}

glc_fetch_remote_raw <- function(x, path) {
  request <- glc_remote_file_request(x, path, error_status = FALSE)
  response <- glc_perform(
    request,
    context = paste0("Download of ", path)
  )
  httr2::resp_body_raw(response)
}

glc_download_remote_raw <- function(x, path, destination) {
  request <- glc_remote_file_request(x, path, error_status = FALSE)
  glc_perform(
    request,
    context = paste0("Download of ", path),
    path = destination
  )
  invisible(destination)
}

glc_repo_tree <- function(x) {
  glc_assert_package(x)
  if (x$source_type != "remote") {
    return(NULL)
  }
  if (!is.null(x$transport$tree)) {
    return(x$transport$tree)
  }

  url <- paste0(
    "https://api.github.com/repos/",
    x$repo,
    "/git/trees/",
    x$commit,
    "?recursive=1"
  )
  result <- glc_get_json(url, token = x$transport$token)
  if (isTRUE(result$truncated)) {
    glc_abort(
      "The GitHub tree for {.val {x$repo}} is truncated. This repository is too large for safe recursive path resolution.",
      class = "glcdp_truncated_tree"
    )
  }
  entries <- result$tree %||% list()
  tree <- if (length(entries) == 0L) {
    tibble::tibble(
      path = character(),
      type = character(),
      size = numeric(),
      sha = character()
    )
  } else {
    tibble::tibble(
      path = vapply(
        entries,
        function(entry) glc_scalar_character(entry$path),
        character(1)
      ),
      type = vapply(
        entries,
        function(entry) glc_scalar_character(entry$type),
        character(1)
      ),
      size = vapply(
        entries,
        function(entry) glc_scalar_number(entry$size),
        numeric(1)
      ),
      sha = vapply(
        entries,
        function(entry) glc_scalar_character(entry$sha),
        character(1)
      )
    )
  }
  x$transport$tree <- tree
  tree
}
