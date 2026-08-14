glc_datetime_compatibility_signature <- function(
  source,
  date,
  date_format,
  time,
  time_format
) {
  source <- as.character(source)
  date <- as.character(date)
  date_format <- as.character(date_format)
  time <- as.character(time)
  time_format <- as.character(time_format)
  collection <- !is.na(source) & source == "collection"
  date[collection] <- "<collection-value>"
  collection_time <- collection & !is.na(time) & nzchar(time)
  time[collection_time] <- "<collection-value>"
  paste(source, date, date_format, time, time_format, sep = "|")
}

glc_factor_level_contracts <- function(data, columns) {
  lapply(data[columns], function(column) {
    if (is.factor(column)) levels(column) else NULL
  })
}

glc_runtime_type_matches <- function(column, type) {
  switch(
    type,
    string = is.character(column),
    boolean = is.logical(column),
    numeric = is.double(column),
    integer = is.integer(column),
    factor = is.factor(column),
    guess = TRUE,
    FALSE
  )
}

glc_collection_validate_factor_payload <- function(payload, data) {
  payload <- glc_validate_runtime_factor_contract(payload)
  columns <- names(data)[!startsWith(names(data), ".glc_")]
  variable_names <- vapply(
    payload$variables,
    function(variable) {
      variable$name
    },
    character(1)
  )
  if (!identical(columns, variable_names)) {
    glc_abort(
      "The collection data columns do not match their factor-contract payload.",
      class = "glcdp_factor_contract_tampered"
    )
  }
  for (variable in payload$variables) {
    column <- data[[variable$name]]
    if (!glc_runtime_type_matches(column, variable$type)) {
      glc_abort(
        "Collection variable {.val {variable$name}} no longer matches its declared type.",
        class = "glcdp_factor_contract_tampered"
      )
    }
    if (identical(variable$type, "factor")) {
      actual_ordered <- is.ordered(column)
      if (
        !identical(actual_ordered, variable$ordered) ||
          !identical(levels(column), variable$factor_labels)
      ) {
        glc_abort(
          "Collection variable {.val {variable$name}} no longer matches its declared factor levels.",
          class = "glcdp_factor_contract_tampered"
        )
      }
    }
  }
  payload
}

glc_collection_harmonize_factors <- function(x) {
  if (!"factor_contract" %in% names(x)) {
    return(x)
  }
  if (!is.list(x$factor_contract) || length(x$factor_contract) != nrow(x)) {
    glc_abort(
      "The collection contains an incomplete factor-contract column.",
      class = "glcdp_factor_contract_invalid"
    )
  }
  payloads <- lapply(seq_len(nrow(x)), function(index) {
    glc_collection_validate_factor_payload(
      x$factor_contract[[index]],
      x$data[[index]]
    )
  })
  variable_names <- lapply(payloads, function(payload) {
    vapply(payload$variables, function(variable) variable$name, character(1))
  })
  if (
    !all(vapply(
      variable_names[-1L],
      identical,
      logical(1),
      variable_names[[1L]]
    ))
  ) {
    return(x)
  }
  for (position in seq_along(payloads[[1L]]$variables)) {
    variables <- lapply(payloads, function(payload) {
      payload$variables[[position]]
    })
    types <- unique(vapply(
      variables,
      function(variable) {
        variable$type
      },
      character(1)
    ))
    if (length(types) != 1L || !identical(types[[1L]], "factor")) {
      next
    }
    union <- glc_factor_union(variables)
    if (!isTRUE(union$compatible)) {
      glc_abort(
        union$message,
        class = c(
          "glcdp_factor_harmonization_conflict",
          "glcdp_incompatible_collection"
        )
      )
    }
    for (index in seq_len(nrow(x))) {
      column <- x$data[[index]][[union$variable_name]]
      x$data[[index]][[union$variable_name]] <- factor(
        as.character(column),
        levels = union$variable$factor_labels,
        ordered = union$variable$ordered
      )
    }
  }
  x
}

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
  reference_factor_levels <- glc_factor_level_contracts(
    reference_data,
    reference_columns
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
    } else if (
      !identical(
        glc_factor_level_contracts(data, columns),
        reference_factor_levels
      )
    ) {
      mismatches <- c(
        mismatches,
        paste0(x$file_group_id[[i]], ": factor levels")
      )
    }
  }
  if (length(unique(x$timezone)) > 1L) mismatches <- c(mismatches, "time zones")
  modality_keys <- vapply(x$modalities, paste, character(1), collapse = "|")
  if (length(unique(modality_keys)) > 1L)
    mismatches <- c(mismatches, "modalities")
  if (length(unique(x$role)) > 1L) mismatches <- c(mismatches, "file roles")
  if (length(unique(x$data_state)) > 1L)
    mismatches <- c(mismatches, "data states")

  relationship_columns <- c(
    "dataset_id",
    "study_id",
    "participant_id",
    "device_id"
  )
  file_group_values <- as.character(x$file_group_id)
  invalid_file_groups <- is.na(file_group_values) | !nzchar(file_group_values)
  if (any(invalid_file_groups)) {
    mismatches <- c(mismatches, "missing file-group identifiers")
  }
  file_group_ids <- unique(file_group_values[!invalid_file_groups])
  conflicting_groups <- file_group_ids[vapply(
    file_group_ids,
    function(file_group_id) {
      rows <- !is.na(x$file_group_id) & x$file_group_id == file_group_id
      nrow(unique(x[rows, relationship_columns, drop = FALSE])) > 1L
    },
    logical(1)
  )]
  if (length(conflicting_groups) > 0L) {
    mismatches <- c(
      mismatches,
      paste0("contradictory relationships for ", conflicting_groups)
    )
  }

  dataset_values <- as.character(x$dataset_id)
  invalid_datasets <- is.na(dataset_values) | !nzchar(dataset_values)
  if (any(invalid_datasets)) {
    mismatches <- c(mismatches, "missing dataset identifiers")
  }
  dataset_ids <- unique(dataset_values[!invalid_datasets])
  conflicting_datasets <- dataset_ids[vapply(
    dataset_ids,
    function(dataset_id) {
      rows <- !is.na(x$dataset_id) & x$dataset_id == dataset_id
      relationships <- unique(
        x[rows, c("study_id", "participant_id"), drop = FALSE]
      )
      nrow(relationships) > 1L
    },
    logical(1)
  )]
  if (length(conflicting_datasets) > 0L) {
    mismatches <- c(
      mismatches,
      paste0("contradictory dataset relationships for ", conflicting_datasets)
    )
  }

  datetime_keys <- glc_datetime_compatibility_signature(
    x$datetime_source,
    x$datetime_date,
    x$datetime_format,
    x$datetime_time,
    x$datetime_time_format
  )
  if (length(unique(datetime_keys)) > 1L) {
    mismatches <- c(mismatches, "datetime specifications")
  }
  unique(mismatches)
}

glc_values_equal <- function(x, y) {
  if (length(x) != length(y)) return(FALSE)
  all(glc_values_match(x, y))
}

glc_values_match <- function(x, y) {
  if (length(x) != length(y)) {
    return(rep(FALSE, max(length(x), length(y))))
  }
  comparison <- x == y
  comparison[is.na(x) & is.na(y)] <- TRUE
  comparison[is.na(comparison)] <- FALSE
  comparison
}

glc_add_standard_column <- function(
  data,
  name,
  value,
  compatible = NULL
) {
  if (name %in% names(data)) {
    existing <- data[[name]]
    equal <- if (identical(name, "Datetime")) {
      inherits(existing, "POSIXct") &&
        glc_values_equal(as.numeric(existing), as.numeric(value))
    } else {
      glc_values_equal(as.character(existing), as.character(value))
    }
    if (!isTRUE(equal) && is.function(compatible)) {
      equal <- isTRUE(compatible(existing, value))
    }
    if (!isTRUE(equal)) {
      glc_abort(
        "Source column {.val {name}} conflicts with the LightLogR-standardized value.",
        class = "glcdp_standard_column_conflict"
      )
    }
  }
  data[[name]] <- value
  data
}

#' Collect compatible file groups
#'
#' Explicitly combines file-group tibbles after checking their columns, types,
#' factor contracts, time zones, modalities, roles, data states, and relationship
#' consistency. Compatible unordered factor declarations are harmonized with the
#' same deterministic union used by [glc_collection_plan()]. Conflicting value,
#' label, description, or order mappings remain blocking. Distinct stable file
#' groups in one dataset may reference different devices because device identity
#' is resolved by `file_group_id`.
#'
#' @param x A collection returned by [glc_read()].
#' @param standardize Either `"lightlogr"` to add the conventional `Id`,
#'   `file_group_id`, `participant_Id`, `Datetime`, and `file.name` columns and
#'   remove internal `.glc_*` provenance columns, or `"none"` to retain source
#'   and provenance columns unchanged.
#'
#' @details
#' Collections returned by current [glc_read()] carry fingerprinted raw factor
#' declarations. `glc_collect()` first verifies those facts against each parsed
#' factor, computes a safe active union, and recasts the factors before binding.
#' Invalid, changed, or conflicting contracts use condition classes
#' `glcdp_factor_contract_invalid`, `glcdp_factor_contract_tampered`, or
#' `glcdp_factor_harmonization_conflict`. The latter also inherits from
#' `glcdp_incompatible_collection`. Legacy `glc_data_collection` objects without
#' a factor-contract payload retain strict exact factor-level comparison.
#'
#' A repeated `file_group_id` must still have one consistent dataset, study,
#' participant, and device relationship. Each dataset must retain consistent
#' study and participant relationships. These runtime checks are not weakened by
#' file-group-scoped device identity.
#'
#' @return A combined tibble. In LightLogR-standardized output, `Id` contains
#'   the dataset id, `file_group_id` identifies the source file group,
#'   `participant_Id` contains the participant id, an existing source
#'   `file.name` column is retained, and the result is grouped by `Id`.
#' @export
#'
#' @examplesIf interactive()
#' iztech <- glc_open("tscnlab/melidos-iztech-glc-dataset")
#' collection <- glc_read(
#'   iztech,
#'   dataset_id = "MELIDOS_IZTECH_S001",
#'   file_group = "MELIDOS_IZTECH_S001:17",
#'   n_max = 10
#' )
#' glc_collect(collection)
glc_collect <- function(x, standardize = c("lightlogr", "none")) {
  if (!inherits(x, "glc_data_collection")) {
    glc_abort("{.arg x} must be returned by {.fn glc_read}.")
  }
  standardize <- match.arg(standardize)
  x <- glc_collection_harmonize_factors(x)
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
  file_group_id <- as.character(result[[".glc_file_group"]])
  participant_id <- as.character(result[[".glc_participant_id"]])
  datetime <- result[[".glc_datetime"]]
  file_name <- basename(result[[".glc_source_file"]])
  result <- glc_add_standard_column(
    result,
    "Id",
    id,
    compatible = function(existing, value) {
      existing <- as.character(existing)
      all(
        glc_values_match(existing, as.character(value)) |
          glc_values_match(existing, participant_id)
      )
    }
  )
  result <- glc_add_standard_column(
    result,
    "file_group_id",
    file_group_id
  )
  result <- glc_add_standard_column(
    result,
    "participant_Id",
    participant_id
  )
  datetime_is_source <- all(
    x$datetime_source == "column" &
      x$datetime_date == "Datetime" &
      (is.na(x$datetime_time) | !nzchar(x$datetime_time))
  )
  result <- glc_add_standard_column(
    result,
    "Datetime",
    datetime,
    compatible = function(existing, value) datetime_is_source
  )
  if ("file.name" %in% names(result)) {
    if (!is.character(result[["file.name"]])) {
      glc_abort(
        "Source column {.val file.name} must be a character column for LightLogR standardization.",
        class = "glcdp_standard_column_conflict"
      )
    }
  } else {
    result <- glc_add_standard_column(result, "file.name", file_name)
  }
  result <- result[, !startsWith(names(result), ".glc_"), drop = FALSE]
  order <- order(as.character(result$Id), result$Datetime, na.last = TRUE)
  result <- result[order, , drop = FALSE]
  dplyr::group_by_at(tibble::as_tibble(result), "Id")
}
