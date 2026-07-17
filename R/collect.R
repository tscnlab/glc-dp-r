glc_collection_mismatches <- function(x) {
  reference_data <- x$data[[1L]]
  reference_columns <- names(reference_data)[
    !startsWith(names(reference_data), ".glc_")
  ]
  reference_classes <- vapply(
    reference_data[reference_columns],
    function(column) paste(class(column), collapse = "/"),
    character(1)
  )
  mismatches <- character()
  for (i in seq_len(nrow(x))[-1L]) {
    data <- x$data[[i]]
    columns <- names(data)[!startsWith(names(data), ".glc_")]
    classes <- vapply(
      data[columns],
      function(column) paste(class(column), collapse = "/"),
      character(1)
    )
    if (!identical(columns, reference_columns)) {
      mismatches <- c(mismatches, paste0(x$file_group_id[[i]], ": columns"))
    } else if (!identical(classes, reference_classes)) {
      mismatches <- c(mismatches, paste0(x$file_group_id[[i]], ": types"))
    }
  }
  if (length(unique(x$timezone)) > 1L) mismatches <- c(mismatches, "time zones")
  modality_keys <- vapply(x$modalities, paste, character(1), collapse = "|")
  if (length(unique(modality_keys)) > 1L)
    mismatches <- c(mismatches, "modalities")
  if (length(unique(x$role)) > 1L) mismatches <- c(mismatches, "file roles")
  if (length(unique(x$data_state)) > 1L)
    mismatches <- c(mismatches, "data states")
  datetime_keys <- paste(
    x$datetime_source,
    x$datetime_date,
    x$datetime_format,
    x$datetime_time,
    x$datetime_time_format,
    sep = "|"
  )
  if (length(unique(datetime_keys)) > 1L) {
    mismatches <- c(mismatches, "datetime specifications")
  }
  unique(mismatches)
}

glc_values_equal <- function(x, y) {
  if (length(x) != length(y)) return(FALSE)
  comparison <- x == y
  comparison[is.na(x) & is.na(y)] <- TRUE
  all(comparison, na.rm = FALSE)
}

glc_add_standard_column <- function(data, name, value) {
  if (name %in% names(data)) {
    existing <- data[[name]]
    equal <- if (identical(name, "Datetime")) {
      inherits(existing, "POSIXct") &&
        glc_values_equal(as.numeric(existing), as.numeric(value))
    } else {
      glc_values_equal(as.character(existing), as.character(value))
    }
    if (!isTRUE(equal)) {
      glc_abort(
        "Source column {.val {name}} conflicts with the LightLogR-standardized value.",
        class = "glcdp_standard_column_conflict"
      )
    }
    return(data)
  }
  data[[name]] <- value
  data
}

#' Collect compatible file groups
#'
#' Explicitly combines file-group tibbles after checking their columns, types,
#' time zones, modalities, roles, and data states.
#'
#' @param x A collection returned by [glc_read()].
#' @param standardize Either `"lightlogr"` to add the conventional `Id`,
#'   `participant_Id`, `Datetime`, and `file.name` columns and remove internal
#'   `.glc_*` provenance columns, or `"none"` to retain source and provenance
#'   columns unchanged.
#'
#' @return A combined tibble. In LightLogR-standardized output, `Id` contains
#'   the dataset id, `participant_Id` contains the participant id, and the
#'   result is grouped by `Id`.
#' @export
glc_collect <- function(x, standardize = c("lightlogr", "none")) {
  if (!inherits(x, "glc_data_collection")) {
    glc_abort("{.arg x} must be returned by {.fn glc_read}.")
  }
  standardize <- match.arg(standardize)
  mismatches <- glc_collection_mismatches(x)
  if (length(mismatches) > 0L) {
    glc_abort(
      "File groups cannot be collected because they differ in {.val {mismatches}}.",
      class = "glcdp_incompatible_collection"
    )
  }
  result <- dplyr::bind_rows(x$data)
  if (identical(standardize, "none")) {
    return(tibble::as_tibble(result))
  }

  id <- as.character(result[[".glc_dataset_id"]])
  id <- factor(id, levels = unique(id))
  participant_id <- as.character(result[[".glc_participant_id"]])
  datetime <- result[[".glc_datetime"]]
  file_name <- basename(result[[".glc_source_file"]])
  result <- glc_add_standard_column(result, "Id", id)
  result <- glc_add_standard_column(
    result,
    "participant_Id",
    participant_id
  )
  result <- glc_add_standard_column(result, "Datetime", datetime)
  result <- glc_add_standard_column(result, "file.name", file_name)
  result <- result[, !startsWith(names(result), ".glc_"), drop = FALSE]
  order <- order(as.character(result$Id), result$Datetime, na.last = TRUE)
  result <- result[order, , drop = FALSE]
  dplyr::group_by_at(tibble::as_tibble(result), "Id")
}
