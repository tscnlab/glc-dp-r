glc_default_registry <- function() {
  getOption(
    "glcdp.registry_url",
    "https://tscnlab.github.io/glc-registry/registry.json"
  )
}

glc_registry_read <- function(registry) {
  if (glc_is_url(registry)) {
    raw <- glc_get_raw(registry, accept = "application/json")
    return(glc_read_json_text(rawToChar(raw), registry))
  }
  glc_read_json_file(registry, simplify = FALSE)
}

glc_registry_row <- function(entry, generated_at) {
  current <- entry$current %||% list()
  latest <- entry$latest_pass %||% list()
  current_status <- glc_scalar_character(
    current$status %||% entry$current_status
  )
  current_commit <- glc_scalar_character(
    current$commit_sha %||% entry$resolved_commit_sha
  )
  latest_commit <- glc_scalar_character(latest$commit_sha)
  repository <- glc_scalar_character(
    entry$repo %||% entry$repository %||% entry$configured_repo
  )

  tibble::tibble(
    id = glc_scalar_character(entry$id, basename(repository)),
    repository = repository,
    branch = glc_scalar_character(entry$branch),
    repository_status = glc_scalar_character(entry$repository_status),
    current_status = current_status,
    current_commit = current_commit,
    current_validator = glc_scalar_character(current$validator_version),
    current_validated_at = glc_scalar_character(current$timestamp),
    current_errors = glc_count_value(current$errors),
    current_warnings = glc_count_value(current$warnings),
    latest_pass_commit = latest_commit,
    latest_pass_validator = glc_scalar_character(latest$validator_version),
    latest_pass_validated_at = glc_scalar_character(latest$timestamp),
    has_latest_pass = !is.na(latest_commit) && nzchar(latest_commit),
    is_current_pass = identical(tolower(current_status), "pass"),
    attestation_verified = glc_scalar_logical(
      entry$attestation_verified %||% current$attestation_verified
    ),
    registry_generated_at = generated_at
  )
}

new_glc_registry <- function(x, generated_at = NA_character_, source = NULL) {
  class(x) <- c("glc_registry", class(x))
  attr(x, "generated_at") <- generated_at
  attr(x, "source") <- source
  x
}

#' List registered Global Light Commons data packages
#'
#' Downloads and flattens the Global Light Commons registry. Both passing and
#' non-passing current revisions are retained.
#'
#' @param registry Registry JSON URL or path. Defaults to the official registry
#'   or the value of option `glcdp.registry_url`.
#' @param refresh Whether to bypass the in-session registry cache.
#'
#' @return A `glc_registry` tibble with one row per registered repository.
#' @export
#'
#' @examplesIf interactive()
#' packages <- glc_packages()
#' glc_search_packages("guidolin", packages)
glc_packages <- function(registry = glc_default_registry(), refresh = FALSE) {
  glc_assert_string(registry, "registry")
  glc_assert_flag(refresh, "refresh")

  if (!refresh && !is.null(.glcdp_state$registry[[registry]])) {
    return(.glcdp_state$registry[[registry]])
  }

  value <- glc_registry_read(registry)
  entries <- value$datasets
  if (!is.list(entries)) {
    glc_abort(
      "The registry must contain a `datasets` array.",
      class = "glcdp_registry_error"
    )
  }
  generated_at <- glc_scalar_character(value$generated_at_utc)
  rows <- lapply(entries, glc_registry_row, generated_at = generated_at)
  result <- if (length(rows) == 0L) {
    glc_registry_row(list(), generated_at)[0, ]
  } else {
    dplyr::bind_rows(rows)
  }
  result <- new_glc_registry(
    result,
    generated_at = generated_at,
    source = registry
  )
  .glcdp_state$registry[[registry]] <- result
  result
}

#' Search registered data packages
#'
#' @param query Optional fixed, case-insensitive text searched in package ids
#'   and repository names.
#' @param packages A registry returned by [glc_packages()].
#' @param status Optional current validation status or statuses.
#' @param has_pass Optional logical value selecting packages with or without a
#'   recorded passing revision.
#'
#' @return A filtered `glc_registry` tibble.
#' @export
glc_search_packages <- function(
  query = NULL,
  packages = glc_packages(),
  status = NULL,
  has_pass = NULL
) {
  if (!inherits(packages, "glc_registry")) {
    glc_abort("{.arg packages} must be returned by {.fn glc_packages}.")
  }
  keep <- rep(TRUE, nrow(packages))
  if (!is.null(query)) {
    glc_assert_string(query, "query", allow_empty = TRUE)
    needle <- tolower(query)
    keep <- keep &
      (grepl(needle, tolower(packages$id), fixed = TRUE) |
        grepl(needle, tolower(packages$repository), fixed = TRUE))
  }
  if (!is.null(status)) {
    if (!is.character(status) || anyNA(status)) {
      glc_abort(
        "{.arg status} must be a character vector without missing values."
      )
    }
    keep <- keep & packages$current_status %in% status
  }
  if (!is.null(has_pass)) {
    glc_assert_flag(has_pass, "has_pass")
    keep <- keep & packages$has_latest_pass == has_pass
  }
  new_glc_registry(
    packages[keep, , drop = FALSE],
    generated_at = attr(packages, "generated_at"),
    source = attr(packages, "source")
  )
}

#' @export
print.glc_registry <- function(x, ..., n = NULL, width = NULL) {
  cat("<GLC registry>\n")
  generated <- attr(x, "generated_at")
  if (!is.null(generated) && !is.na(generated)) {
    cat("Generated:", generated, "\n")
  }
  NextMethod("print", x, ..., n = n, width = width)
  invisible(x)
}
