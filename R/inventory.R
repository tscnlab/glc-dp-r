#' Inventory data-package resources
#'
#' @param x A package opened with [glc_open()].
#'
#' @return A tibble with one row per declared resource path.
#' @export
glc_resources <- function(x) {
  glc_assert_package(x)
  resources <- x$descriptor$resources %||% list()
  rows <- list()
  row_index <- 0L
  for (descriptor in resources) {
    name <- glc_scalar_character(descriptor$name)
    paths <- glc_compact_character(descriptor$path)
    if (length(paths) == 0L) paths <- NA_character_
    for (path in paths) {
      row_index <- row_index + 1L
      dialect <- descriptor$dialect %||% list()
      rows[[row_index]] <- tibble::tibble(
        resource = name,
        path = path,
        core = name %in% glc_core_resource_names(),
        directory = !is.na(path) && endsWith(path, "/"),
        format = glc_scalar_character(descriptor$format),
        media_type = glc_scalar_character(
          descriptor$mediatype %||% descriptor$mediaType
        ),
        profile = glc_scalar_character(descriptor$profile),
        schema_path = glc_scalar_character(
          descriptor$schema %||% descriptor$jsonSchema
        ),
        delimiter = glc_scalar_character(dialect$delimiter),
        decimal_mark = glc_scalar_character(
          dialect$decimalChar %||% dialect$decimal_mark
        )
      )
    }
  }
  if (length(rows) == 0L) {
    return(tibble::tibble(
      resource = character(),
      path = character(),
      core = logical(),
      directory = logical(),
      format = character(),
      media_type = character(),
      profile = character(),
      schema_path = character(),
      delimiter = character(),
      decimal_mark = character()
    ))
  }
  dplyr::bind_rows(rows)
}

glc_selected_datasets <- function(x, dataset_id = NULL) {
  datasets <- glc_model(x)$datasets
  if (is.null(dataset_id) || identical(dataset_id, "all")) {
    return(datasets)
  }
  if (!is.character(dataset_id) || anyNA(dataset_id)) {
    glc_abort(
      "{.arg dataset_id} must be a character vector without missing values."
    )
  }
  available <- vapply(datasets, function(dataset) dataset$id, character(1))
  missing <- setdiff(dataset_id, available)
  if (length(missing) > 0L) {
    glc_abort(
      "Unknown dataset id{?s}: {.val {missing}}. Available ids are {.val {available}}.",
      class = "glcdp_unknown_dataset"
    )
  }
  datasets[available %in% dataset_id]
}

#' Inventory datasets
#'
#' @param x A package opened with [glc_open()].
#' @param dataset_id Optional dataset id or ids.
#'
#' @return A tibble with one row per dataset.
#' @export
glc_datasets <- function(x, dataset_id = NULL) {
  glc_assert_package(x)
  datasets <- glc_selected_datasets(x, dataset_id)
  rows <- lapply(datasets, function(dataset) {
    groups <- dataset$groups
    tibble::tibble(
      dataset_id = dataset$id,
      schema_version = dataset$schema_version,
      study_id = dataset$study_id,
      participant_id = dataset$participant_id,
      participant_associated = dataset$participant_associated,
      timezone = dataset$timezone,
      latitude = dataset$latitude,
      longitude = dataset$longitude,
      file_group_count = length(groups),
      file_count = sum(vapply(
        groups,
        function(group) length(group$files),
        integer(1)
      )),
      modalities = list(glc_unique_chr(lapply(
        groups,
        function(group) group$modality
      ))),
      device_ids = list(glc_unique_chr(lapply(
        groups,
        function(group) group$device_id
      ))),
      primary_variables = list(glc_unique_chr(
        lapply(groups, function(group) group$primary_variables)
      ))
    )
  })
  if (length(rows) == 0L) {
    return(tibble::tibble(
      dataset_id = character(),
      schema_version = character(),
      study_id = character(),
      participant_id = character(),
      participant_associated = logical(),
      timezone = character(),
      latitude = numeric(),
      longitude = numeric(),
      file_group_count = integer(),
      file_count = integer(),
      modalities = list(),
      device_ids = list(),
      primary_variables = list()
    ))
  }
  dplyr::bind_rows(rows)
}

glc_group_selected <- function(group, file_group, role, modality) {
  keep <- TRUE
  if (!is.null(file_group)) {
    keep <- keep &&
      (group$id %in%
        as.character(file_group) ||
        group$index %in% suppressWarnings(as.integer(file_group)))
  }
  if (!is.null(role)) keep <- keep && group$role %in% role
  if (!is.null(modality)) keep <- keep && any(group$modality %in% modality)
  keep
}

#' Inventory declared data files
#'
#' @param x A package opened with [glc_open()].
#' @param dataset_id Optional dataset id or ids.
#' @param file_group Optional group index or stable `dataset:group` id.
#' @param role Optional file-group role.
#' @param modality Optional modality.
#' @param available Optional logical filter for file availability.
#'
#' @return A tibble with one row per concrete declared file.
#' @export
glc_files <- function(
  x,
  dataset_id = NULL,
  file_group = NULL,
  role = NULL,
  modality = NULL,
  available = NULL
) {
  glc_assert_package(x)
  if (!is.null(available)) glc_assert_flag(available, "available")
  datasets <- glc_selected_datasets(x, dataset_id)
  rows <- list()
  row_index <- 0L
  for (dataset in datasets) {
    for (group in dataset$groups) {
      if (!glc_group_selected(group, file_group, role, modality)) next
      for (declared_path in group$files) {
        info <- tryCatch(
          glc_file_info_internal(x, declared_path),
          glcdp_missing_path = function(cnd)
            list(
              path = declared_path,
              storage = NA_character_,
              expected_size = NA_real_,
              lfs_oid = NA_character_,
              blob_sha = NA_character_,
              available = FALSE
            )
        )
        row_index <- row_index + 1L
        rows[[row_index]] <- tibble::tibble(
          dataset_id = dataset$id,
          file_group = group$index,
          file_group_id = group$id,
          participant_id = dataset$participant_id,
          study_id = dataset$study_id,
          path = info$path,
          declared_path = declared_path,
          format = group$format,
          encoding = group$encoding,
          timezone = group$timezone,
          role = group$role,
          data_state = group$data_state,
          modalities = list(group$modality),
          device_id = group$device_id,
          storage = info$storage,
          expected_bytes = info$expected_size,
          lfs_oid = info$lfs_oid,
          blob_sha = info$blob_sha,
          available = info$available
        )
      }
    }
  }
  if (length(rows) == 0L) {
    result <- tibble::tibble(
      dataset_id = character(),
      file_group = integer(),
      file_group_id = character(),
      participant_id = character(),
      study_id = character(),
      path = character(),
      declared_path = character(),
      format = character(),
      encoding = character(),
      timezone = character(),
      role = character(),
      data_state = character(),
      modalities = list(),
      device_id = character(),
      storage = character(),
      expected_bytes = numeric(),
      lfs_oid = character(),
      blob_sha = character(),
      available = logical()
    )
  } else {
    result <- dplyr::bind_rows(rows)
  }
  if (!is.null(available)) {
    result <- result[result$available == available, , drop = FALSE]
  }
  result
}

#' Inventory and search declared variables
#'
#' @param x A package opened with [glc_open()].
#' @param dataset_id Optional dataset id or ids.
#' @param file_group Optional group index or stable id.
#' @param term Optional semantic term or terms.
#' @param primary Optional logical filter for primary variables.
#'
#' @return A tibble with one row per declared variable.
#' @export
glc_variables <- function(
  x,
  dataset_id = NULL,
  file_group = NULL,
  term = NULL,
  primary = NULL
) {
  glc_assert_package(x)
  if (!is.null(primary)) glc_assert_flag(primary, "primary")
  datasets <- glc_selected_datasets(x, dataset_id)
  rows <- list()
  row_index <- 0L
  for (dataset in datasets) {
    for (group in dataset$groups) {
      if (!glc_group_selected(group, file_group, role = NULL, modality = NULL))
        next
      for (variable in group$variables) {
        if (!is.null(term) && !variable$term %in% term) next
        if (!is.null(primary) && !identical(variable$primary, primary)) next
        levels <- variable$factor_levels
        row_index <- row_index + 1L
        rows[[row_index]] <- tibble::tibble(
          dataset_id = dataset$id,
          file_group = group$index,
          file_group_id = group$id,
          name = variable$name,
          label = variable$label,
          unit = variable$unit,
          type = variable$type,
          term = variable$term,
          term_name = variable$term_name,
          calibration = variable$calibration,
          primary = variable$primary,
          factor_values = list(vapply(
            levels,
            function(level) level$value,
            character(1)
          )),
          factor_labels = list(vapply(
            levels,
            function(level) level$label,
            character(1)
          ))
        )
      }
    }
  }
  if (length(rows) == 0L) {
    return(tibble::tibble(
      dataset_id = character(),
      file_group = integer(),
      file_group_id = character(),
      name = character(),
      label = character(),
      unit = character(),
      type = character(),
      term = character(),
      term_name = character(),
      calibration = character(),
      primary = logical(),
      factor_values = list(),
      factor_labels = list()
    ))
  }
  dplyr::bind_rows(rows)
}

glc_optional_resource_records <- function(x, name) {
  tryCatch(
    glc_records(glc_read_named_resource_raw(x, name)),
    glcdp_missing_resource = function(cnd) list(),
    glcdp_missing_path = function(cnd) list()
  )
}

#' Summarize a Global Light Commons data package
#'
#' @param x A package opened with [glc_open()].
#'
#' @return A one-row `glc_summary` tibble. For local packages, declared and
#'   locally available dataset, file-group, and file counts are reported
#'   separately.
#' @export
glc_summary <- function(x) {
  glc_assert_package(x)
  datasets <- glc_model(x)$datasets
  files <- glc_files(x)
  groups <- unlist(
    lapply(datasets, function(dataset) dataset$groups),
    recursive = FALSE
  )
  participants <- glc_optional_resource_records(x, "participants")
  devices <- glc_optional_resource_records(x, "devices")
  studies <- glc_optional_resource_records(x, "study")
  available_files <- files$available %in% TRUE
  available_dataset_ids <- unique(files$dataset_id[available_files])
  available_group_ids <- unique(files$file_group_id[available_files])
  result <- tibble::tibble(
    schema_version = x$schema_version,
    availability_checked = identical(x$source_type, "local"),
    study_count = length(studies),
    dataset_count = length(datasets),
    available_dataset_count = length(available_dataset_ids),
    participant_count = length(participants),
    device_count = length(devices),
    file_group_count = length(groups),
    available_file_group_count = length(available_group_ids),
    file_count = nrow(files),
    available_file_count = sum(available_files),
    missing_file_count = sum(!available_files),
    variable_count = nrow(glc_variables(x)),
    declared_bytes = sum(files$expected_bytes, na.rm = TRUE),
    modalities = list(glc_unique_chr(lapply(
      groups,
      function(group) group$modality
    ))),
    timezones = list(glc_unique_chr(lapply(
      groups,
      function(group) group$timezone
    ))),
    primary_variables = list(glc_unique_chr(
      lapply(groups, function(group) group$primary_variables)
    ))
  )
  class(result) <- c("glc_summary", class(result))
  result
}

#' @export
print.glc_summary <- function(x, ...) {
  cat("<GLC package summary>\n")
  cat("Schema: ", x$schema_version[[1L]], "\n", sep = "")
  format_availability <- function(available, declared) {
    if (isTRUE(x$availability_checked[[1L]])) {
      paste0(available, " available / ", declared, " declared")
    } else {
      as.character(declared)
    }
  }
  cat(
    "Studies: ",
    x$study_count[[1L]],
    " | Datasets: ",
    format_availability(
      x$available_dataset_count[[1L]],
      x$dataset_count[[1L]]
    ),
    " | Participants: ",
    x$participant_count[[1L]],
    "\n",
    sep = ""
  )
  cat(
    "File groups: ",
    format_availability(
      x$available_file_group_count[[1L]],
      x$file_group_count[[1L]]
    ),
    " | Files: ",
    format_availability(
      x$available_file_count[[1L]],
      x$file_count[[1L]]
    ),
    " | Variables: ",
    x$variable_count[[1L]],
    "\n",
    sep = ""
  )
  invisible(x)
}
