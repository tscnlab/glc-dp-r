glc_core_resource_names <- function() {
  c(
    "study",
    "participants",
    "participant_characteristics",
    "datasets",
    "devices",
    "device_datasheets",
    "contributors"
  )
}

glc_resource_descriptor <- function(x, name) {
  resources <- x$descriptor$resources %||% list()
  names <- vapply(
    resources,
    function(resource) glc_scalar_character(resource$name),
    character(1)
  )
  index <- which(names == name)
  if (length(index) == 0L) {
    glc_abort(
      "Data package does not declare a resource named {.val {name}}.",
      class = "glcdp_missing_resource"
    )
  }
  if (length(index) > 1L) {
    glc_abort("Data package declares resource {.val {name}} more than once.")
  }
  resources[[index]]
}

glc_local_paths <- function(x) {
  if (!is.null(x$transport$local_paths)) {
    return(x$transport$local_paths)
  }
  paths <- list.files(
    x$root,
    recursive = TRUE,
    all.files = TRUE,
    no.. = TRUE,
    include.dirs = TRUE
  )
  paths <- gsub("\\\\", "/", paths)
  paths <- paths[!grepl("(^|/)\\.git(/|$)", paths)]
  x$transport$local_paths <- paths
  paths
}

glc_all_paths <- function(x, type = c("any", "blob", "tree")) {
  type <- match.arg(type)
  if (x$source_type == "local") {
    paths <- glc_local_paths(x)
    if (type == "blob") {
      paths <- paths[
        file.exists(file.path(x$root, paths)) &
          !dir.exists(file.path(x$root, paths))
      ]
    } else if (type == "tree") {
      paths <- paths[dir.exists(file.path(x$root, paths))]
    }
    return(paths)
  }
  tree <- glc_repo_tree(x)
  if (type == "any") tree$path else tree$path[tree$type == type]
}

glc_resolve_path <- function(x, path, allow_directory = FALSE) {
  path <- glc_safe_path(path)
  blobs <- glc_all_paths(x, "blob")
  trees <- if (allow_directory) glc_all_paths(x, "tree") else character()
  candidates <- c(blobs, trees)
  exact <- candidates[candidates == sub("/$", "", path)]
  if (length(exact) == 1L) {
    return(exact)
  }

  basename_matches <- blobs[basename(blobs) == basename(path)]
  if (length(basename_matches) == 1L) {
    return(basename_matches)
  }
  if (length(basename_matches) > 1L) {
    glc_abort(
      "Legacy path {.path {path}} is ambiguous; matching files are {.path {basename_matches}}.",
      class = "glcdp_ambiguous_path"
    )
  }
  glc_abort(
    "Declared package path does not exist at the selected revision: {.path {path}}.",
    class = "glcdp_missing_path"
  )
}

glc_expand_declared_path <- function(x, path) {
  path <- glc_safe_path(path)
  prefix <- sub("/$", "", path)
  is_directory <- endsWith(path, "/") || prefix %in% glc_all_paths(x, "tree")
  if (!is_directory) {
    return(glc_resolve_path(x, path))
  }
  blobs <- glc_all_paths(x, "blob")
  matches <- blobs[startsWith(blobs, paste0(prefix, "/"))]
  if (length(matches) == 0L) {
    glc_abort("Declared directory is empty or missing: {.path {path}}.")
  }
  sort(matches)
}

glc_fetch_path_raw <- function(x, path) {
  path <- glc_resolve_path(x, path)
  if (x$source_type == "local") {
    file_path <- file.path(x$root, path)
    connection <- file(file_path, open = "rb")
    on.exit(close(connection), add = TRUE)
    return(readBin(connection, what = "raw", n = file.info(file_path)$size))
  }
  glc_fetch_remote_raw(x, path)
}

glc_read_named_resource_raw <- function(x, name) {
  cache_key <- paste0("resource:", name)
  cached <- x$transport$resource_raw[[cache_key]]
  if (!is.null(cached)) {
    return(cached)
  }
  resource <- glc_resource_descriptor(x, name)
  paths <- glc_compact_character(resource$path)
  if (length(paths) == 0L) {
    glc_abort("Resource {.val {name}} does not declare a path.")
  }
  expanded <- unlist(
    lapply(paths, function(path) glc_expand_declared_path(x, path)),
    use.names = FALSE
  )
  values <- lapply(expanded, function(path) {
    raw <- glc_fetch_path_raw(x, path)
    glc_read_json_text(rawToChar(raw), path)
  })
  value <- if (length(values) == 1L) values[[1L]] else
    stats::setNames(values, expanded)
  x$transport$resource_raw[[cache_key]] <- value
  value
}

glc_normalize_levels <- function(levels) {
  levels <- glc_records(levels)
  lapply(levels, function(level) {
    list(
      value = glc_scalar_character(level$value),
      label = glc_scalar_character(
        level$label,
        glc_scalar_character(level$value)
      ),
      description = glc_scalar_character(level$description)
    )
  })
}

glc_normalize_file_encodings <- function(encodings, file_count, version) {
  values <- as.character(unlist(
    encodings,
    recursive = TRUE,
    use.names = FALSE
  ))
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0L) {
    if (version %in% glc_typed_schema_versions()) {
      glc_abort(
        "Schema {version} file group does not declare `dataset_file_encoding`.",
        class = "glcdp_file_metadata"
      )
    }
    values <- "UTF-8"
  }
  if (file_count == 0L) {
    return(character())
  }
  if (length(values) == 1L) {
    return(rep(values, file_count))
  }
  if (length(values) != file_count) {
    glc_abort(
      "File group declares {file_count} file{?s} but {length(values)} encoding value{?s}.",
      class = "glcdp_file_metadata"
    )
  }
  values
}

glc_normalize_variable <- function(variable, primary_variables, version) {
  term <- variable$dataset_file_variables_term %||% list()
  name <- glc_scalar_character(variable$dataset_file_variables_name)
  type <- glc_scalar_character(
    variable$dataset_file_variables_type,
    if (version %in% c("1.0.0", "2.0.0")) "guess" else NA_character_
  )
  factor_levels <- glc_normalize_levels(
    variable$dataset_file_variables_factor_levels
  )
  if (version %in% glc_typed_schema_versions() && is.na(type)) {
    glc_abort(
      "Schema {version} variable {.val {name}} does not declare `dataset_file_variables_type`.",
      class = "glcdp_variable_metadata"
    )
  }
  supported_types <- c(
    "string",
    "boolean",
    "numeric",
    "integer",
    "factor",
    "guess"
  )
  if (!type %in% supported_types) {
    glc_abort(
      "Variable {.val {name}} declares unsupported type {.val {type}}.",
      class = "glcdp_variable_metadata"
    )
  }
  if (identical(type, "factor") && length(factor_levels) == 0L) {
    glc_abort(
      "Factor variable {.val {name}} does not declare any factor levels.",
      class = "glcdp_variable_metadata"
    )
  }
  list(
    name = name,
    label = glc_scalar_character(variable$dataset_file_variables_labels),
    description = glc_scalar_character(
      variable$dataset_file_variables_description
    ),
    unit = glc_scalar_character(variable$dataset_file_variables_units),
    calibration = glc_scalar_character(
      variable$dataset_file_variables_calibration
    ),
    type = type,
    factor_levels = factor_levels,
    term = glc_scalar_character(term$variable_term),
    term_name = glc_scalar_character(term$variable_name),
    primary = name %in% primary_variables
  )
}

glc_infer_modalities <- function(variables) {
  terms <- tolower(vapply(
    variables,
    function(variable) variable$term,
    character(1)
  ))
  modalities <- character()
  if (any(grepl("light|illuminance|edi", terms), na.rm = TRUE)) {
    modalities <- c(modalities, "light")
  }
  if (any(grepl("acceler", terms), na.rm = TRUE)) {
    modalities <- c(modalities, "accelerometry")
  }
  if (any(grepl("temperature", terms), na.rm = TRUE)) {
    modalities <- c(modalities, "temperature")
  }
  if (length(modalities) == 0L) "unspecified" else unique(modalities)
}

glc_normalize_group <- function(group, dataset, version, index) {
  primary_variables <- glc_compact_character(group$primary_variables)
  variables <- lapply(
    glc_records(group$dataset_file_variables),
    glc_normalize_variable,
    primary_variables = primary_variables,
    version = version
  )
  legacy <- version %in% c("1.0.0", "2.0.0")
  crossref <- dataset$dataset_crossref %||% list()
  preprocessing <- group$dataset_file_preprocessing %||% list()
  preprocessed <- isTRUE(preprocessing$dataset_file_preprocessing_bol)
  datetime <- if (legacy) {
    dataset$dataset_datetime %||% list()
  } else {
    group$dataset_file_datetime %||% list()
  }
  temporal <- group$dataset_file_temporal_resolution %||% list()
  sampling <- glc_scalar_number(dataset$dataset_sampling_interval)
  files <- glc_compact_character(group$dataset_file_names)
  encodings <- glc_normalize_file_encodings(
    group$dataset_file_encoding,
    length(files),
    version
  )
  modality <- glc_compact_character(group$dataset_file_modality)
  if (length(modality) == 0L) {
    modality <- glc_infer_modalities(variables)
  }

  list(
    index = as.integer(index),
    id = paste(
      glc_scalar_character(dataset$dataset_internal_id),
      index,
      sep = ":"
    ),
    modality = modality,
    modality_other = glc_scalar_character(group$dataset_file_modality_other),
    modality_other_type = glc_scalar_character(
      group$dataset_file_modality_other_type
    ),
    description = glc_scalar_character(group$dataset_file_description),
    device_id = glc_scalar_character(
      group$dataset_file_crossref_device_id %||%
        crossref$dataset_crossref_device_id
    ),
    device_location = glc_scalar_character(
      group$dataset_file_device_location %||%
        dataset$dataset_device_location
    ),
    device_location_type = glc_scalar_character(
      group$dataset_file_device_location_type
    ),
    temporal_type = glc_scalar_character(
      temporal$resolution_type,
      if (!is.na(sampling)) "fixed_interval" else NA_character_
    ),
    temporal_value = glc_scalar_number(
      temporal$value,
      sampling
    ),
    temporal_unit = glc_scalar_character(
      temporal$unit,
      if (!is.na(sampling)) "seconds" else NA_character_
    ),
    instructions = glc_scalar_character(
      group$dataset_file_instructions %||% dataset$dataset_instructions
    ),
    files = files,
    format = tolower(glc_scalar_character(group$dataset_file_format)),
    encodings = encodings,
    timezone = glc_scalar_character(
      group$dataset_file_timezone %||% dataset$dataset_timezone
    ),
    datetime = list(
      source = glc_scalar_character(
        datetime$dataset_file_datetime_source,
        "column"
      ),
      date = glc_scalar_character(
        datetime$dataset_file_datetime_date %||%
          datetime$dataset_datetime_date
      ),
      date_format = glc_scalar_character(
        datetime$dataset_file_datetime_dateformat %||%
          datetime$dataset_datetime_dateformat
      ),
      time = glc_scalar_character(
        datetime$dataset_file_datetime_time %||%
          datetime$dataset_datetime_time
      ),
      time_format = glc_scalar_character(
        datetime$dataset_file_datetime_timeformat %||%
          datetime$dataset_datetime_timeformat
      )
    ),
    role = glc_scalar_character(
      group$dataset_file_role,
      if (isTRUE(group$dataset_file_auxiliary)) "supporting" else "primary"
    ),
    data_state = glc_scalar_character(
      group$dataset_file_data_state,
      if (preprocessed) "processed" else "raw"
    ),
    header_row = as.integer(glc_scalar_number(group$dataset_file_header_row)),
    preprocessing = glc_compact_character(
      preprocessing$dataset_file_preprocessing_desc
    ),
    instrument = group$dataset_file_instrument %||% list(),
    variables = variables,
    primary_variables = primary_variables
  )
}

glc_normalize_dataset <- function(dataset, version) {
  crossref <- dataset$dataset_crossref %||% list()
  participant_id <- glc_scalar_character(
    crossref$dataset_crossref_participant_id
  )
  participant_associated <- if (
    !is.null(dataset$dataset_participant_associated)
  ) {
    isTRUE(dataset$dataset_participant_associated)
  } else {
    !is.na(participant_id) && nzchar(participant_id)
  }
  location <- suppressWarnings(as.numeric(
    glc_compact_character(dataset$dataset_location)
  ))
  groups <- glc_records(dataset$dataset_file)
  groups <- lapply(seq_along(groups), function(index) {
    glc_normalize_group(groups[[index]], dataset, version, index)
  })

  list(
    id = glc_scalar_character(dataset$dataset_internal_id),
    schema_version = glc_scalar_character(dataset$schema_version, version),
    study_id = glc_scalar_character(crossref$dataset_crossref_study_id),
    participant_id = participant_id,
    participant_associated = participant_associated,
    timezone = glc_scalar_character(dataset$dataset_timezone),
    latitude = if (length(location) >= 1L) location[[1L]] else NA_real_,
    longitude = if (length(location) >= 2L) location[[2L]] else NA_real_,
    variable_terms = glc_records(dataset$dataset_variable_terms),
    groups = groups,
    raw = dataset
  )
}

glc_check_dataset_schema_versions <- function(dataset_records, version) {
  record_versions <- vapply(
    dataset_records,
    function(dataset) glc_scalar_character(dataset$schema_version),
    character(1)
  )
  record_versions <- unique(
    record_versions[!is.na(record_versions) & nzchar(record_versions)]
  )
  if (length(record_versions) > 1L) {
    glc_abort(
      "Dataset records declare multiple GLC schema versions: {.val {record_versions}}.",
      class = "glcdp_schema_error"
    )
  }
  if (
    length(record_versions) == 1L &&
      !identical(record_versions[[1L]], version)
  ) {
    glc_abort(
      paste0(
        "Dataset schema version `{record_versions[[1L]]}` conflicts with ",
        "package schema version `{version}`."
      ),
      class = "glcdp_schema_error"
    )
  }
  invisible(version)
}

glc_model <- function(x) {
  glc_assert_package(x)
  if (!is.null(x$transport$model)) {
    return(x$transport$model)
  }
  datasets_raw <- glc_read_named_resource_raw(x, "datasets")
  dataset_records <- glc_records(datasets_raw)
  glc_check_dataset_schema_versions(dataset_records, x$schema_version)
  datasets <- lapply(
    dataset_records,
    glc_normalize_dataset,
    version = x$schema_version
  )
  ids <- vapply(datasets, function(dataset) dataset$id, character(1))
  if (anyNA(ids) || any(!nzchar(ids)) || anyDuplicated(ids)) {
    glc_abort(
      "Dataset identifiers must be non-empty and unique.",
      class = "glcdp_dataset_id_error"
    )
  }
  model <- list(datasets = datasets)
  x$transport$model <- model
  model
}
