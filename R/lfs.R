glc_parse_lfs_pointer <- function(value) {
  text <- if (is.raw(value)) rawToChar(value) else as.character(value)
  text <- sub("\\x00.*$", "", text)
  lines <- strsplit(gsub("\\r\\n?", "\n", text), "\n", fixed = TRUE)[[1L]]
  if (
    length(lines) == 0L ||
      !identical(lines[[1L]], "version https://git-lfs.github.com/spec/v1")
  ) {
    return(NULL)
  }
  oid_line <- grep("^oid sha256:[0-9a-fA-F]{64}$", lines, value = TRUE)
  size_line <- grep("^size [0-9]+$", lines, value = TRUE)
  if (length(oid_line) != 1L || length(size_line) != 1L) {
    glc_abort(
      "A Git LFS pointer is malformed.",
      class = "glcdp_malformed_lfs_pointer"
    )
  }
  list(
    oid = tolower(sub("^oid sha256:", "", oid_line)),
    size = as.numeric(sub("^size ", "", size_line))
  )
}

glc_read_prefix <- function(path, n = 1024L) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)
  readBin(connection, what = "raw", n = n)
}

glc_manifest_entry <- function(x, path) {
  entries <- x$manifest$files %||% list()
  if (length(entries) == 0L) {
    return(NULL)
  }
  index <- which(vapply(
    entries,
    function(entry) identical(glc_scalar_character(entry$path), path),
    logical(1)
  ))
  if (length(index) == 1L) entries[[index]] else NULL
}

glc_file_info_internal <- function(x, path) {
  path <- glc_resolve_path(x, path)
  if (x$source_type == "local") {
    full_path <- file.path(x$root, path)
    size <- unname(file.info(full_path)$size)
    parsed_pointer <- if (size <= 1024) {
      glc_parse_lfs_pointer(glc_read_prefix(full_path))
    } else {
      NULL
    }
    pointer <- parsed_pointer
    manifest_entry <- glc_manifest_entry(x, path)
    if (!is.null(manifest_entry) && identical(manifest_entry$storage, "lfs")) {
      pointer <- list(
        oid = glc_scalar_character(manifest_entry$lfs_oid),
        size = glc_scalar_number(manifest_entry$bytes, size)
      )
    }
    return(list(
      path = path,
      storage = if (is.null(pointer)) "git" else "lfs",
      expected_size = if (is.null(pointer)) size else pointer$size,
      lfs_oid = if (is.null(pointer)) NA_character_ else pointer$oid,
      blob_sha = NA_character_,
      available = file.exists(full_path),
      local_pointer = !is.null(parsed_pointer)
    ))
  }

  tree <- glc_repo_tree(x)
  row <- tree[tree$path == path & tree$type == "blob", , drop = FALSE]
  if (nrow(row) != 1L) {
    glc_abort("Could not locate remote blob for {.path {path}}.")
  }
  pointer <- NULL
  if (!is.na(row$size[[1L]]) && row$size[[1L]] <= 1024) {
    pointer <- glc_parse_lfs_pointer(glc_fetch_remote_raw(x, path))
  }
  list(
    path = path,
    storage = if (is.null(pointer)) "git" else "lfs",
    expected_size = if (is.null(pointer)) row$size[[1L]] else pointer$size,
    lfs_oid = if (is.null(pointer)) NA_character_ else pointer$oid,
    blob_sha = row$sha[[1L]],
    available = TRUE,
    local_pointer = FALSE
  )
}

glc_check_external_lfs <- function(x) {
  paths <- glc_all_paths(x, "blob")
  if (!".lfsconfig" %in% paths) {
    return(invisible(FALSE))
  }
  glc_abort(
    "Repository {.val {x$repo}} declares `.lfsconfig`. External Git LFS servers are not supported in this release.",
    class = "glcdp_external_lfs"
  )
}

glc_lfs_batch_action <- function(x, oid, size) {
  if (is.na(x$repo) || !nzchar(x$repo)) {
    glc_abort(
      "The local package contains a Git LFS pointer but its source repository is unknown.",
      class = "glcdp_lfs_repository_unknown"
    )
  }
  glc_check_external_lfs(x)
  url <- paste0(
    "https://github.com/",
    x$repo,
    ".git/info/lfs/objects/batch"
  )
  body <- list(
    operation = "download",
    transfers = list("basic"),
    objects = list(list(oid = oid, size = size))
  )
  request <- glc_request(
    url,
    token = x$transport$token,
    accept = "application/vnd.git-lfs+json",
    error_status = FALSE
  ) |>
    httr2::req_headers(`Content-Type` = "application/vnd.git-lfs+json") |>
    httr2::req_body_json(body)
  response <- glc_perform(request, context = "Git LFS batch request")
  value <- tryCatch(
    glc_read_json_text(
      rawToChar(httr2::resp_body_raw(response)),
      "Git LFS batch response"
    ),
    error = function(cnd) {
      glc_abort(
        "Git LFS returned malformed JSON.",
        class = "glcdp_lfs_response"
      )
    }
  )
  objects <- value$objects %||% list()
  if (length(objects) == 0L) {
    glc_abort(
      "Git LFS returned no object result for {.val {oid}}.",
      class = "glcdp_lfs_response"
    )
  }
  object <- objects[[1L]] %||% list()
  if (!is.null(object$error)) {
    code <- glc_scalar_number(object$error$code)
    message <- glc_scalar_character(object$error$message, "Object unavailable")
    condition_class <- if (identical(code, 404)) {
      "glcdp_lfs_missing_object"
    } else if (code %in% c(401, 403, 429)) {
      "glcdp_lfs_auth_or_quota"
    } else {
      "glcdp_lfs_object_error"
    }
    glc_abort(
      "Git LFS object {.val {oid}} is unavailable ({code}): {message}.",
      class = condition_class
    )
  }
  action <- object$actions$download
  if (is.null(action$href)) {
    glc_abort(
      "Git LFS did not provide a download action for object {.val {oid}}.",
      class = "glcdp_lfs_response"
    )
  }
  action
}

glc_lfs_action_request <- function(action) {
  request <- glc_request(
    glc_scalar_character(action$href),
    token = NULL,
    accept = "application/octet-stream",
    error_status = FALSE
  )
  headers <- action$header %||% list()
  if (length(headers) > 0L) {
    header_values <- lapply(headers, glc_scalar_character)
    request <- do.call(
      httr2::req_headers,
      c(list(.req = request), header_values)
    )
  }
  request
}

glc_lfs_perform_action <- function(action, destination) {
  request <- glc_lfs_action_request(action)
  response <- tryCatch(
    httr2::req_perform(request, path = destination),
    error = function(cnd) {
      glc_abort(
        "Git LFS object download failed because the storage service could not be reached.",
        class = "glcdp_lfs_network_error"
      )
    }
  )
  status <- httr2::resp_status(response)
  if (status %in% c(200L, 206L) && !file.exists(destination)) {
    writeBin(httr2::resp_body_raw(response), destination)
  }
  status
}

glc_verify_lfs <- function(path, oid, size) {
  actual_size <- unname(file.info(path)$size)
  if (
    is.na(actual_size) || !identical(as.numeric(actual_size), as.numeric(size))
  ) {
    glc_abort(
      "Git LFS size verification failed: expected {size} bytes, received {actual_size}.",
      class = "glcdp_lfs_size_mismatch"
    )
  }
  actual_oid <- digest::digest(
    path,
    algo = "sha256",
    serialize = FALSE,
    file = TRUE
  )
  if (!identical(tolower(actual_oid), tolower(oid))) {
    glc_abort(
      "Git LFS hash verification failed for object {.val {oid}}.",
      class = "glcdp_lfs_hash_mismatch"
    )
  }
  invisible(actual_oid)
}

glc_download_lfs <- function(x, info, destination) {
  action <- glc_lfs_batch_action(x, info$lfs_oid, info$expected_size)
  status <- glc_lfs_perform_action(action, destination)
  if (status %in% c(401L, 403L)) {
    unlink(destination)
    action <- glc_lfs_batch_action(x, info$lfs_oid, info$expected_size)
    status <- glc_lfs_perform_action(action, destination)
  }
  if (!status %in% c(200L, 206L)) {
    condition_class <- if (status %in% c(401L, 403L, 429L)) {
      "glcdp_lfs_auth_or_quota"
    } else {
      "glcdp_lfs_download_error"
    }
    glc_abort(
      "Git LFS object download failed with HTTP status {status}.",
      class = condition_class
    )
  }
  glc_verify_lfs(destination, info$lfs_oid, info$expected_size)
  invisible(destination)
}

glc_transfer_to <- function(x, path, destination, info = NULL) {
  info <- info %||% glc_file_info_internal(x, path)
  if (
    identical(info$storage, "lfs") &&
      (x$source_type == "remote" || isTRUE(info$local_pointer))
  ) {
    glc_download_lfs(x, info, destination)
  } else if (x$source_type == "local") {
    copied <- file.copy(
      file.path(x$root, info$path),
      destination,
      overwrite = TRUE,
      copy.mode = TRUE
    )
    if (!copied) {
      glc_abort("Could not copy local package file {.path {info$path}}.")
    }
  } else {
    if (info$expected_size > 100 * 1024^2) {
      glc_abort(
        "Ordinary Git blob {.path {info$path}} is larger than GitHub's 100 MB Git blob limit. Files of this size must be stored with Git LFS.",
        class = "glcdp_unsupported_storage"
      )
    }
    glc_download_remote_raw(x, info$path, destination)
  }

  actual_size <- unname(file.info(destination)$size)
  sha256 <- digest::digest(
    destination,
    algo = "sha256",
    serialize = FALSE,
    file = TRUE
  )
  list(
    path = info$path,
    storage = info$storage,
    bytes = actual_size,
    sha256 = sha256,
    lfs_oid = info$lfs_oid
  )
}

glc_atomic_transfer <- function(x, path, destination, overwrite = FALSE) {
  glc_assert_flag(overwrite, "overwrite")
  if (file.exists(destination) && !overwrite) {
    glc_abort(
      "Destination file already exists: {.path {destination}}. Set `overwrite = TRUE` to replace it.",
      class = "glcdp_file_exists"
    )
  }
  dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(".glcdp-", tmpdir = dirname(destination))
  on.exit(unlink(temporary), add = TRUE)
  info <- glc_file_info_internal(x, path)
  result <- glc_transfer_to(x, info$path, temporary, info = info)
  if (file.exists(destination)) {
    unlink(destination)
  }
  if (!file.rename(temporary, destination)) {
    glc_abort("Could not finalize download at {.path {destination}}.")
  }
  result
}

glc_cache_root <- function(x) {
  root <- x$transport$cache_dir %||% file.path(tempdir(), "glcdp-cache")
  identity <- paste(x$repo, x$commit, sep = "@")
  key <- digest::digest(identity, algo = "sha256", serialize = FALSE)
  file.path(root, key)
}

glc_materialize_file <- function(x, path) {
  path <- glc_resolve_path(x, path)
  if (x$source_type == "local") {
    info <- glc_file_info_internal(x, path)
    if (!isTRUE(info$local_pointer)) {
      return(file.path(x$root, path))
    }
  }
  destination <- file.path(glc_cache_root(x), path)
  if (file.exists(destination)) {
    return(destination)
  }
  glc_atomic_transfer(x, path, destination, overwrite = FALSE)
  destination
}
