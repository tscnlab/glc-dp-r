glc_collection_metadata_schema <- function() {
  "glc-package-metadata"
}

glc_collection_metadata_version <- function() {
  "1.0.0"
}

glc_plan_metadata_unwrap <- function(x) {
  while (
    is.list(x) &&
      !is.data.frame(x) &&
      length(x) == 1L &&
      (is.null(names(x)) || !nzchar(names(x)[[1L]]))
  ) {
    x <- x[[1L]]
  }
  x
}

glc_plan_metadata_field <- function(record, aliases, default = NULL) {
  if (!is.list(record) || is.null(names(record))) {
    return(default)
  }
  index <- match(aliases, names(record), nomatch = 0L)
  index <- index[index > 0L]
  if (length(index) == 0L) {
    return(default)
  }
  glc_plan_metadata_unwrap(record[[index[[1L]]]])
}

glc_plan_metadata_character <- function(record, aliases) {
  glc_scalar_character(glc_plan_metadata_field(record, aliases))
}

glc_plan_metadata_number <- function(record, aliases) {
  glc_scalar_number(glc_plan_metadata_field(record, aliases))
}

glc_plan_metadata_integer <- function(record, aliases) {
  value <- glc_plan_metadata_number(record, aliases)
  if (is.na(value)) NA_integer_ else as.integer(value)
}

glc_plan_metadata_characters <- function(record, aliases) {
  glc_compact_character(glc_plan_metadata_field(record, aliases))
}

glc_plan_metadata_records <- function(value, split_top = FALSE) {
  if (is.null(value)) {
    return(list())
  }
  if (split_top && is.list(value) && !is.data.frame(value)) {
    return(unlist(lapply(value, glc_records), recursive = FALSE))
  }
  glc_records(value)
}

glc_plan_metadata_nested_records <- function(record, aliases) {
  value <- glc_plan_metadata_field(record, aliases)
  if (is.null(value)) list() else glc_records(value)
}

glc_plan_metadata_atomic_values <- function(x) {
  x <- glc_plan_metadata_unwrap(x)
  if (is.null(x) || length(x) == 0L) {
    return(list(NA_character_))
  }
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (is.atomic(x)) {
    return(lapply(seq_along(x), function(index) x[[index]]))
  }
  if (is.list(x)) {
    values <- unlist(
      lapply(x, glc_plan_metadata_atomic_values),
      recursive = FALSE
    )
    if (length(values) == 0L) list(NA_character_) else values
  } else {
    list(glc_plan_safe_metadata(x))
  }
}

glc_plan_metadata_value_type <- function(x) {
  if (inherits(x, "Date")) {
    return("date")
  }
  if (inherits(x, "POSIXt")) {
    return("datetime")
  }
  switch(
    typeof(x),
    logical = "logical",
    integer = "integer",
    double = "double",
    character = "character",
    raw = "raw",
    "unknown"
  )
}

glc_plan_metadata_order <- function(x, columns) {
  if (nrow(x) < 2L || length(columns) == 0L) {
    return(x)
  }
  keys <- lapply(columns, function(column) {
    value <- x[[column]]
    if (is.character(value)) glc_plan_utf8_key(value) else value
  })
  x[do.call(order, c(keys, list(method = "radix"))), , drop = FALSE]
}

glc_plan_metadata_validate_ids <- function(x, column, resource) {
  values <- x[[column]]
  if (length(values) == 0L) {
    return(invisible(TRUE))
  }
  invalid <- is.na(values) | !nzchar(values)
  if (any(invalid) || anyDuplicated(values)) {
    glc_abort(
      "Core metadata resource {.val {resource}} has missing or duplicate stable identifiers.",
      class = "glcdp_collection_plan_metadata"
    )
  }
  invisible(TRUE)
}

glc_plan_metadata_resource_descriptors <- function(x) {
  descriptors <- x$descriptor$resources %||% list()
  names <- vapply(
    descriptors,
    function(resource) glc_scalar_character(resource$name),
    character(1)
  )
  stats::setNames(descriptors, names)
}

glc_plan_metadata_load_resources <- function(x) {
  core <- glc_core_resource_names()
  descriptors <- glc_plan_metadata_resource_descriptors(x)
  declared <- intersect(core, names(descriptors))
  requested <- setdiff(declared, "datasets")
  loaded <- x$transport$collection_plan_core_metadata
  cache_valid <- is.list(loaded) && identical(names(loaded), requested)
  if (!cache_valid && length(requested) > 0L) {
    loaded <- tryCatch(
      glc_metadata(x, resources = requested),
      error = function(cnd) {
        glc_abort(
          paste0(
            "Could not load the exact-revision core metadata snapshot: ",
            conditionMessage(cnd)
          ),
          class = "glcdp_collection_plan_metadata",
          parent = cnd
        )
      }
    )
    x$transport$collection_plan_core_metadata <- loaded
  } else if (!cache_valid) {
    loaded <- list()
    x$transport$collection_plan_core_metadata <- loaded
  }
  records <- stats::setNames(vector("list", length(core)), core)
  counts <- stats::setNames(integer(length(core)), core)
  for (resource in requested) {
    descriptor <- descriptors[[resource]]
    paths <- glc_compact_character(descriptor$path)
    split_top <- length(paths) > 1L || any(endsWith(paths, "/"))
    records[[resource]] <- glc_plan_metadata_records(
      loaded[[resource]],
      split_top = split_top
    )
    counts[[resource]] <- length(records[[resource]])
  }
  list(
    core = core,
    declared = declared,
    records = records,
    counts = counts
  )
}

glc_plan_empty_resource_status <- function() {
  tibble::tibble(
    resource = character(),
    declared = logical(),
    status = character(),
    record_count = integer()
  )
}

glc_plan_metadata_resource_status <- function(loaded, dataset_count) {
  counts <- loaded$counts
  counts[["datasets"]] <- dataset_count
  tibble::tibble(
    resource = loaded$core,
    declared = loaded$core %in% loaded$declared,
    status = vapply(
      loaded$core,
      function(resource) {
        if (!resource %in% loaded$declared) {
          "not_declared"
        } else if (counts[[resource]] == 0L) {
          "loaded_empty"
        } else {
          "loaded"
        }
      },
      character(1)
    ),
    record_count = as.integer(counts[loaded$core])
  )
}

glc_plan_empty_studies <- function() {
  tibble::tibble(
    study_id = character(),
    schema_version = character(),
    title = character(),
    short_description = character(),
    preregistration = character(),
    registration = character(),
    ethics = character(),
    sample = character(),
    intervention = character(),
    setting = character(),
    geographical_location = character(),
    study_type = character(),
    funding_sources = list(),
    keywords = list(),
    dataset_ids = list()
  )
}

glc_plan_empty_study_groups <- function() {
  tibble::tibble(
    study_id = character(),
    position = integer(),
    name = character(),
    description = character(),
    size = integer(),
    inclusion = list(),
    exclusion = list(),
    dataset_ids = list()
  )
}

glc_plan_empty_contributors <- function(study = FALSE) {
  if (study) {
    return(tibble::tibble(
      study_id = character(),
      position = integer(),
      full_name = character(),
      roles = list(),
      email = character(),
      orcid = character(),
      institution_name = character(),
      institution_city = character(),
      institution_country = character()
    ))
  }
  tibble::tibble(
    contributor_id = character(),
    position = integer(),
    full_name = character(),
    roles = list(),
    email = character(),
    orcid = character(),
    institution_name = character(),
    institution_city = character(),
    institution_country = character()
  )
}

glc_plan_metadata_institution <- function(record) {
  institution <- glc_plan_metadata_field(
    record,
    c("contributor_institution", "institution")
  )
  nested <- glc_records(institution)
  if (length(nested) == 0L) {
    return(list(
      name = NA_character_,
      city = NA_character_,
      country = NA_character_
    ))
  }
  value <- nested[[1L]]
  list(
    name = glc_plan_metadata_character(
      value,
      c("contributor_institution_name", "institution_name", "name")
    ),
    city = glc_plan_metadata_character(
      value,
      c("contributor_institution_city", "institution_city", "city")
    ),
    country = glc_plan_metadata_character(
      value,
      c("contributor_institution_country", "institution_country", "country")
    )
  )
}

glc_plan_metadata_contributor_row <- function(
  record,
  position,
  study_id = NULL
) {
  institution <- glc_plan_metadata_institution(record)
  values <- list(
    position = as.integer(position),
    full_name = glc_plan_metadata_character(
      record,
      c("contributor_full_name", "full_name", "name")
    ),
    roles = list(glc_plan_metadata_characters(
      record,
      c("contributor_roles", "roles")
    )),
    email = glc_plan_metadata_character(
      record,
      c("contributor_email", "email")
    ),
    orcid = glc_plan_metadata_character(
      record,
      c("contributor_orcid", "orcid")
    ),
    institution_name = institution$name,
    institution_city = institution$city,
    institution_country = institution$country
  )
  if (is.null(study_id)) {
    values <- c(
      list(
        contributor_id = glc_plan_metadata_character(
          record,
          c("contributor_internal_id", "contributor_id")
        )
      ),
      values
    )
  } else {
    values <- c(list(study_id = study_id), values)
  }
  tibble::as_tibble(values)
}

glc_plan_metadata_studies <- function(records) {
  study_rows <- list()
  group_rows <- list()
  contributor_rows <- list()
  for (position in seq_along(records)) {
    record <- records[[position]]
    study_id <- glc_plan_metadata_character(
      record,
      c("study_internal_id", "study_id")
    )
    study_rows[[length(study_rows) + 1L]] <- tibble::tibble(
      study_id = study_id,
      schema_version = glc_plan_metadata_character(record, "schema_version"),
      title = glc_plan_metadata_character(record, c("study_title", "title")),
      short_description = glc_plan_metadata_character(
        record,
        c("study_short_description", "short_description")
      ),
      preregistration = glc_plan_metadata_character(
        record,
        c("study_preregistration", "preregistration")
      ),
      registration = glc_plan_metadata_character(
        record,
        c("study_registration", "registration")
      ),
      ethics = glc_plan_metadata_character(record, c("study_ethics", "ethics")),
      sample = glc_plan_metadata_character(record, c("study_sample", "sample")),
      intervention = glc_plan_metadata_character(
        record,
        c("study_intervention", "intervention")
      ),
      setting = glc_plan_metadata_character(
        record,
        c("study_setting", "setting")
      ),
      geographical_location = glc_plan_metadata_character(
        record,
        c("study_geographical_location", "geographical_location")
      ),
      study_type = glc_plan_metadata_character(
        record,
        c("study_type", "type")
      ),
      funding_sources = list(glc_plan_metadata_characters(
        record,
        c("study_funding_sources", "funding_sources")
      )),
      keywords = list(glc_plan_metadata_characters(
        record,
        c("study_keywords", "keywords")
      )),
      dataset_ids = list(glc_plan_metadata_characters(
        record,
        c("study_datasets", "dataset_ids")
      ))
    )
    groups <- glc_plan_metadata_nested_records(
      record,
      c("study_groups", "groups")
    )
    for (group_position in seq_along(groups)) {
      group <- groups[[group_position]]
      group_rows[[length(group_rows) + 1L]] <- tibble::tibble(
        study_id = study_id,
        position = as.integer(group_position),
        name = glc_plan_metadata_character(
          group,
          c("study_group_name", "name")
        ),
        description = glc_plan_metadata_character(
          group,
          c("study_group_description", "description")
        ),
        size = glc_plan_metadata_integer(
          group,
          c("study_group_size", "size")
        ),
        inclusion = list(glc_plan_metadata_characters(
          group,
          c("study_group_inclusion", "inclusion")
        )),
        exclusion = list(glc_plan_metadata_characters(
          group,
          c("study_group_exclusion", "exclusion")
        )),
        dataset_ids = list(glc_plan_metadata_characters(
          group,
          c("study_group_datasets", "dataset_ids")
        ))
      )
    }
    contributors <- glc_plan_metadata_nested_records(
      record,
      c("study_contributors", "contributors")
    )
    for (contributor_position in seq_along(contributors)) {
      contributor_rows[[length(contributor_rows) + 1L]] <-
        glc_plan_metadata_contributor_row(
          contributors[[contributor_position]],
          contributor_position,
          study_id = study_id
        )
    }
  }
  studies <- if (length(study_rows) == 0L) {
    glc_plan_empty_studies()
  } else {
    dplyr::bind_rows(study_rows)
  }
  groups <- if (length(group_rows) == 0L) {
    glc_plan_empty_study_groups()
  } else {
    dplyr::bind_rows(group_rows)
  }
  contributors <- if (length(contributor_rows) == 0L) {
    glc_plan_empty_contributors(study = TRUE)
  } else {
    dplyr::bind_rows(contributor_rows)
  }
  glc_plan_metadata_validate_ids(studies, "study_id", "study")
  list(
    studies = glc_plan_metadata_order(studies, "study_id"),
    study_groups = glc_plan_metadata_order(groups, c("study_id", "position")),
    study_contributors = glc_plan_metadata_order(
      contributors,
      c("study_id", "position")
    )
  )
}

glc_plan_metadata_contributors <- function(records) {
  if (length(records) == 0L) {
    return(glc_plan_empty_contributors())
  }
  rows <- lapply(seq_along(records), function(position) {
    glc_plan_metadata_contributor_row(records[[position]], position)
  })
  dplyr::bind_rows(rows)
}

glc_plan_empty_datasets <- function() {
  tibble::tibble(
    dataset_id = character(),
    schema_version = character(),
    study_id = character(),
    study_link_status = character(),
    participant_id = character(),
    participant_associated = logical(),
    participant_link_status = character(),
    timezone = character(),
    latitude = numeric(),
    longitude = numeric(),
    file_group_count = integer(),
    file_count = integer(),
    modalities = list(),
    device_ids = list(),
    primary_variables = list()
  )
}

glc_plan_empty_dataset_terms <- function() {
  tibble::tibble(
    dataset_id = character(),
    position = integer(),
    term = character(),
    label = character()
  )
}

glc_plan_metadata_datasets <- function(model) {
  rows <- list()
  terms <- list()
  for (dataset in model$datasets) {
    rows[[length(rows) + 1L]] <- tibble::tibble(
      dataset_id = dataset$id,
      schema_version = dataset$schema_version,
      study_id = dataset$study_id,
      study_link_status = NA_character_,
      participant_id = dataset$participant_id,
      participant_associated = dataset$participant_associated,
      participant_link_status = NA_character_,
      timezone = dataset$timezone,
      latitude = dataset$latitude,
      longitude = dataset$longitude,
      file_group_count = length(dataset$groups),
      file_count = sum(vapply(
        dataset$groups,
        function(group) length(group$files),
        integer(1)
      )),
      modalities = list(glc_unique_chr(lapply(
        dataset$groups,
        function(group) group$modality
      ))),
      device_ids = list(glc_unique_chr(lapply(
        dataset$groups,
        function(group) group$device_id
      ))),
      primary_variables = list(glc_unique_chr(lapply(
        dataset$groups,
        function(group) group$primary_variables
      )))
    )
    dataset_terms <- dataset$variable_terms
    for (position in seq_along(dataset_terms)) {
      term <- dataset_terms[[position]]
      terms[[length(terms) + 1L]] <- tibble::tibble(
        dataset_id = dataset$id,
        position = as.integer(position),
        term = glc_plan_metadata_character(
          term,
          c("term", "variable_term")
        ),
        label = glc_plan_metadata_character(
          term,
          c("label", "variable_name")
        )
      )
    }
  }
  datasets <- if (length(rows) == 0L) {
    glc_plan_empty_datasets()
  } else {
    dplyr::bind_rows(rows)
  }
  dataset_terms <- if (length(terms) == 0L) {
    glc_plan_empty_dataset_terms()
  } else {
    dplyr::bind_rows(terms)
  }
  list(
    datasets = glc_plan_metadata_order(datasets, "dataset_id"),
    dataset_terms = glc_plan_metadata_order(
      dataset_terms,
      c("dataset_id", "position")
    )
  )
}

glc_plan_empty_participants <- function() {
  tibble::tibble(
    participant_id = character(),
    age = numeric(),
    sex = character(),
    gender = character()
  )
}

glc_plan_metadata_participants <- function(records) {
  if (length(records) == 0L) {
    return(glc_plan_empty_participants())
  }
  rows <- lapply(records, function(record) {
    tibble::tibble(
      participant_id = glc_plan_metadata_character(
        record,
        c("participant_internal_id", "participant_id")
      ),
      age = glc_plan_metadata_number(record, c("participant_age", "age")),
      sex = glc_plan_metadata_character(record, c("participant_sex", "sex")),
      gender = glc_plan_metadata_character(
        record,
        c("participant_gender", "gender")
      )
    )
  })
  result <- dplyr::bind_rows(rows)
  glc_plan_metadata_validate_ids(result, "participant_id", "participants")
  glc_plan_metadata_order(result, "participant_id")
}

glc_plan_empty_participant_characteristics <- function() {
  tibble::tibble(
    participant_id = character(),
    participant_link_status = character(),
    characteristic_position = integer(),
    value_position = integer(),
    name = character(),
    value = list(),
    value_type = character(),
    unit = character(),
    description = character()
  )
}

glc_plan_metadata_participant_characteristics <- function(records) {
  rows <- list()
  for (position in seq_along(records)) {
    record <- records[[position]]
    values <- glc_plan_metadata_atomic_values(glc_plan_metadata_field(
      record,
      c(
        "participant_characteristic_value",
        "participant_characteristic_values",
        "characteristic_value"
      )
    ))
    for (value_position in seq_along(values)) {
      value <- values[[value_position]]
      value_type <- glc_plan_metadata_value_type(value)
      rows[[length(rows) + 1L]] <- tibble::tibble(
        participant_id = glc_plan_metadata_character(
          record,
          c(
            "participant_internal_id",
            "participant_id",
            "participant_characteristic_participant_id",
            "participant_characteristic_crossref_participant_id"
          )
        ),
        participant_link_status = NA_character_,
        characteristic_position = as.integer(position),
        value_position = as.integer(value_position),
        name = glc_plan_metadata_character(
          record,
          c("participant_characteristic_name", "characteristic_name")
        ),
        value = list(value),
        value_type = value_type,
        unit = glc_plan_metadata_character(
          record,
          c("participant_characteristic_unit", "characteristic_unit")
        ),
        description = glc_plan_metadata_character(
          record,
          c(
            "participant_characteristic_description",
            "characteristic_description"
          )
        )
      )
    }
  }
  if (length(rows) == 0L) {
    return(glc_plan_empty_participant_characteristics())
  }
  glc_plan_metadata_order(
    dplyr::bind_rows(rows),
    c("participant_id", "characteristic_position", "value_position")
  )
}

glc_plan_empty_devices <- function() {
  tibble::tibble(
    device_id = character(),
    schema_version = character(),
    manufacturer = character(),
    model = character(),
    serial_number = character(),
    calibration_date = character(),
    firmware_version = character(),
    datasheet_id = character(),
    datasheet_link_status = character()
  )
}

glc_plan_empty_device_sensors <- function() {
  tibble::tibble(
    device_id = character(),
    position = integer(),
    sensor_type = character(),
    datasheet_id = character(),
    datasheet_link_status = character()
  )
}

glc_plan_metadata_devices <- function(records) {
  device_rows <- list()
  sensor_rows <- list()
  for (record in records) {
    device_id <- glc_plan_metadata_character(
      record,
      c("device_internal_id", "device_id")
    )
    device_rows[[length(device_rows) + 1L]] <- tibble::tibble(
      device_id = device_id,
      schema_version = glc_plan_metadata_character(record, "schema_version"),
      manufacturer = glc_plan_metadata_character(
        record,
        c("device_manufacturer", "manufacturer")
      ),
      model = glc_plan_metadata_character(record, c("device_model", "model")),
      serial_number = glc_plan_metadata_character(
        record,
        c("device_serial_number", "serial_number")
      ),
      calibration_date = glc_plan_metadata_character(
        record,
        c("device_calibration_date", "calibration_date")
      ),
      firmware_version = glc_plan_metadata_character(
        record,
        c("device_firmware_version", "firmware_version")
      ),
      datasheet_id = glc_plan_metadata_character(
        record,
        c("device_datasheet_id", "datasheet_id")
      ),
      datasheet_link_status = NA_character_
    )
    sensors <- glc_plan_metadata_nested_records(
      record,
      c("device_sensors", "sensors")
    )
    for (position in seq_along(sensors)) {
      sensor_rows[[length(sensor_rows) + 1L]] <- tibble::tibble(
        device_id = device_id,
        position = as.integer(position),
        sensor_type = glc_plan_metadata_character(
          sensors[[position]],
          c("device_sensor_type", "sensor_type", "type")
        ),
        datasheet_id = glc_plan_metadata_character(
          sensors[[position]],
          c("device_sensor_datasheet_id", "datasheet_id")
        ),
        datasheet_link_status = NA_character_
      )
    }
  }
  devices <- if (length(device_rows) == 0L) {
    glc_plan_empty_devices()
  } else {
    dplyr::bind_rows(device_rows)
  }
  sensors <- if (length(sensor_rows) == 0L) {
    glc_plan_empty_device_sensors()
  } else {
    dplyr::bind_rows(sensor_rows)
  }
  glc_plan_metadata_validate_ids(devices, "device_id", "devices")
  list(
    devices = glc_plan_metadata_order(devices, "device_id"),
    device_sensors = glc_plan_metadata_order(
      sensors,
      c("device_id", "position")
    )
  )
}

glc_plan_empty_datasheets <- function() {
  tibble::tibble(
    datasheet_id = character(),
    schema_version = character(),
    datasheet_version = character(),
    manufacturer = character(),
    type = character(),
    modalities = list(),
    modality_other = character(),
    model = character(),
    calibration_interval = integer(),
    calibration_method = character(),
    calibration_accuracy = character(),
    calibration_range = character(),
    calibration_notes = character(),
    calibration_spectral_sensitivity = list(),
    calibration_linearity = character(),
    calibration_directional_response = character()
  )
}

glc_plan_empty_datasheet_parameters <- function() {
  tibble::tibble(
    datasheet_id = character(),
    position = integer(),
    name = character(),
    value = list(),
    value_type = character(),
    unit = character(),
    description = character()
  )
}

glc_plan_empty_datasheet_channels <- function() {
  tibble::tibble(
    datasheet_id = character(),
    position = integer(),
    channel_number = integer(),
    name = character(),
    description = character(),
    unit = character()
  )
}

glc_plan_metadata_datasheets <- function(records) {
  datasheet_rows <- list()
  parameter_rows <- list()
  channel_rows <- list()
  for (record in records) {
    datasheet_id <- glc_plan_metadata_character(
      record,
      c("datasheet_id", "device_datasheet_internal_id")
    )
    spectral_sensitivity <- lapply(
      glc_plan_metadata_nested_records(
        record,
        "datasheet_calibration_spectral_sensitivity"
      ),
      function(point) {
        list(
          wavelength = glc_plan_metadata_number(
            point,
            "datasheet_calibration_spectral_sensitivity_wavelength"
          ),
          relative = glc_plan_metadata_number(
            point,
            "datasheet_calibration_spectral_sensitivity_relative"
          )
        )
      }
    )
    datasheet_rows[[length(datasheet_rows) + 1L]] <- tibble::tibble(
      datasheet_id = datasheet_id,
      schema_version = glc_plan_metadata_character(record, "schema_version"),
      datasheet_version = glc_plan_metadata_character(
        record,
        "datasheet_version"
      ),
      manufacturer = glc_plan_metadata_character(
        record,
        c("datasheet_manufacturer", "manufacturer")
      ),
      type = glc_plan_metadata_character(record, c("datasheet_type", "type")),
      modalities = list(glc_plan_metadata_characters(
        record,
        c("datasheet_sensor_modality", "sensor_modality", "modalities")
      )),
      modality_other = glc_plan_metadata_character(
        record,
        c("datasheet_sensor_modality_other", "modality_other")
      ),
      model = glc_plan_metadata_character(
        record,
        c("datasheet_model", "model")
      ),
      calibration_interval = glc_plan_metadata_integer(
        record,
        "datasheet_calibration_interval"
      ),
      calibration_method = glc_plan_metadata_character(
        record,
        c("datasheet_calibration_method", "calibration_method")
      ),
      calibration_accuracy = glc_plan_metadata_character(
        record,
        c("datasheet_calibration_accuracy", "calibration_accuracy")
      ),
      calibration_range = glc_plan_metadata_character(
        record,
        c("datasheet_calibration_range", "calibration_range")
      ),
      calibration_notes = glc_plan_metadata_character(
        record,
        c("datasheet_calibration_notes", "calibration_notes")
      ),
      calibration_spectral_sensitivity = list(spectral_sensitivity),
      calibration_linearity = glc_plan_metadata_character(
        record,
        "datasheet_calibration_linearity"
      ),
      calibration_directional_response = glc_plan_metadata_character(
        record,
        "datasheet_calibration_directional_response"
      )
    )
    parameters <- glc_plan_metadata_nested_records(
      record,
      c("datasheet_calibration_parameters", "calibration_parameters")
    )
    for (position in seq_along(parameters)) {
      parameter <- parameters[[position]]
      value <- glc_plan_metadata_field(
        parameter,
        c("parameter_value", "value"),
        NA_character_
      )
      values <- glc_plan_metadata_atomic_values(value)
      if (length(values) != 1L) {
        glc_abort(
          "A datasheet calibration parameter must have one scalar value.",
          class = "glcdp_collection_plan_metadata"
        )
      }
      parameter_rows[[length(parameter_rows) + 1L]] <- tibble::tibble(
        datasheet_id = datasheet_id,
        position = as.integer(position),
        name = glc_plan_metadata_character(
          parameter,
          c("parameter_name", "name")
        ),
        value = list(values[[1L]]),
        value_type = glc_plan_metadata_value_type(values[[1L]]),
        unit = glc_plan_metadata_character(
          parameter,
          c("parameter_unit", "unit")
        ),
        description = glc_plan_metadata_character(
          parameter,
          c("parameter_description", "description")
        )
      )
    }
    channels <- glc_plan_metadata_nested_records(
      record,
      c("datasheet_channel", "channels")
    )
    for (position in seq_along(channels)) {
      channel <- channels[[position]]
      channel_rows[[length(channel_rows) + 1L]] <- tibble::tibble(
        datasheet_id = datasheet_id,
        position = as.integer(position),
        channel_number = glc_plan_metadata_integer(
          channel,
          c("datasheet_channel_nr", "channel_number", "number")
        ),
        name = glc_plan_metadata_character(
          channel,
          c("datasheet_channel_name", "channel_name", "name")
        ),
        description = glc_plan_metadata_character(
          channel,
          c(
            "datasheet_channel_description",
            "channel_description",
            "description"
          )
        ),
        unit = glc_plan_metadata_character(
          channel,
          c("datasheet_channel_unit", "channel_unit", "unit")
        )
      )
    }
  }
  datasheets <- if (length(datasheet_rows) == 0L) {
    glc_plan_empty_datasheets()
  } else {
    dplyr::bind_rows(datasheet_rows)
  }
  parameters <- if (length(parameter_rows) == 0L) {
    glc_plan_empty_datasheet_parameters()
  } else {
    dplyr::bind_rows(parameter_rows)
  }
  channels <- if (length(channel_rows) == 0L) {
    glc_plan_empty_datasheet_channels()
  } else {
    dplyr::bind_rows(channel_rows)
  }
  glc_plan_metadata_validate_ids(
    datasheets,
    "datasheet_id",
    "device_datasheets"
  )
  list(
    datasheets = glc_plan_metadata_order(datasheets, "datasheet_id"),
    datasheet_parameters = glc_plan_metadata_order(
      parameters,
      c("datasheet_id", "position")
    ),
    datasheet_channels = glc_plan_metadata_order(
      channels,
      c("datasheet_id", "position")
    )
  )
}

glc_plan_metadata_resource_available <- function(metadata, resource) {
  row <- match(resource, metadata$resource_status$resource)
  metadata$resource_status$status[[row]] %in% c("loaded", "loaded_empty")
}

glc_plan_metadata_link_status <- function(
  id,
  applicable,
  available,
  known_ids
) {
  if (!isTRUE(applicable)) {
    return("not_applicable")
  }
  if (is.na(id) || !nzchar(id)) {
    return("unresolved")
  }
  if (!available) {
    return("metadata_unavailable")
  }
  if (id %in% known_ids) "linked" else "unresolved"
}

glc_plan_metadata_apply_links <- function(metadata, records) {
  study_available <- glc_plan_metadata_resource_available(metadata, "study")
  participant_available <- glc_plan_metadata_resource_available(
    metadata,
    "participants"
  )
  device_available <- glc_plan_metadata_resource_available(metadata, "devices")
  datasheet_available <- glc_plan_metadata_resource_available(
    metadata,
    "device_datasheets"
  )
  study_ids <- metadata$studies$study_id
  participant_ids <- metadata$participants$participant_id
  device_ids <- metadata$devices$device_id
  datasheet_ids <- metadata$datasheets$datasheet_id

  metadata$datasets$study_link_status <- unname(vapply(
    metadata$datasets$study_id,
    function(id)
      glc_plan_metadata_link_status(
        id,
        !is.na(id) && nzchar(id),
        study_available,
        study_ids
      ),
    character(1)
  ))
  metadata$datasets$participant_link_status <- unname(vapply(
    seq_len(nrow(metadata$datasets)),
    function(index)
      glc_plan_metadata_link_status(
        metadata$datasets$participant_id[[index]],
        metadata$datasets$participant_associated[[index]],
        participant_available,
        participant_ids
      ),
    character(1)
  ))
  if (nrow(metadata$participant_characteristics) > 0L) {
    metadata$participant_characteristics$participant_link_status <- unname(vapply(
      metadata$participant_characteristics$participant_id,
      function(id)
        glc_plan_metadata_link_status(
          id,
          TRUE,
          participant_available,
          participant_ids
        ),
      character(1)
    ))
  }
  if (nrow(metadata$devices) > 0L) {
    metadata$devices$datasheet_link_status <- unname(vapply(
      metadata$devices$datasheet_id,
      function(id)
        glc_plan_metadata_link_status(
          id,
          !is.na(id) && nzchar(id),
          datasheet_available,
          datasheet_ids
        ),
      character(1)
    ))
  }
  if (nrow(metadata$device_sensors) > 0L) {
    metadata$device_sensors$datasheet_link_status <- unname(vapply(
      metadata$device_sensors$datasheet_id,
      function(id)
        glc_plan_metadata_link_status(
          id,
          !is.na(id) && nzchar(id),
          datasheet_available,
          datasheet_ids
        ),
      character(1)
    ))
  }

  for (index in seq_along(records)) {
    record <- records[[index]]
    record$study_link_status <- glc_plan_metadata_link_status(
      record$study_id,
      !is.na(record$study_id) && nzchar(record$study_id),
      study_available,
      study_ids
    )
    record$participant_link_status <- glc_plan_metadata_link_status(
      record$participant_id,
      record$participant_associated,
      participant_available,
      participant_ids
    )
    record$device_link_status <- glc_plan_metadata_link_status(
      record$device_id,
      !is.na(record$device_id) && nzchar(record$device_id),
      device_available,
      device_ids
    )
    device_index <- match(record$device_id, metadata$devices$device_id)
    record$datasheet_id <- if (is.na(device_index)) {
      NA_character_
    } else {
      metadata$devices$datasheet_id[[device_index]]
    }
    records[[index]] <- record
  }
  list(metadata = metadata, records = records)
}

glc_plan_empty_instruments <- function() {
  tibble::tibble(
    dataset_id = character(),
    file_group_id = character(),
    instrument_type = character(),
    instrument_name = character(),
    collection_method = character(),
    recorded_by = character(),
    software_name = character()
  )
}

glc_plan_empty_file_group_variables <- function() {
  tibble::tibble(
    dataset_id = character(),
    file_group_id = character(),
    declaration_position = integer(),
    selected_by_request = logical(),
    selected_for_output = logical(),
    selection_origin = character(),
    name = character(),
    label = character(),
    description = character(),
    unit = character(),
    calibration = character(),
    type = character(),
    term = character(),
    term_name = character(),
    primary = logical(),
    factor_level_count = integer()
  )
}

glc_plan_empty_file_group_factor_levels <- function() {
  tibble::tibble(
    dataset_id = character(),
    file_group_id = character(),
    variable_position = integer(),
    variable_name = character(),
    level_position = integer(),
    value = character(),
    label = character(),
    description = character()
  )
}

glc_plan_metadata_group_tables <- function(records, variable_scope) {
  records <- records[vapply(
    records,
    function(record) identical(record$status, "included"),
    logical(1)
  )]
  selection_origin <- switch(
    variable_scope,
    matched = "matched_term",
    all = "all_declared",
    selected = "selected_name"
  )
  instrument_records <- records[vapply(
    records,
    function(record) length(record$instrument) > 0L,
    logical(1)
  )]
  instrument_table <- if (length(instrument_records) == 0L) {
    glc_plan_empty_instruments()
  } else {
    tibble::tibble(
      dataset_id = vapply(
        instrument_records,
        function(record) record$dataset_id,
        character(1)
      ),
      file_group_id = vapply(
        instrument_records,
        function(record) record$file_group_id,
        character(1)
      ),
      instrument_type = vapply(
        instrument_records,
        function(record)
          glc_plan_metadata_character(
            record$instrument,
            "instrument_type"
          ),
        character(1)
      ),
      instrument_name = vapply(
        instrument_records,
        function(record)
          glc_plan_metadata_character(
            record$instrument,
            "instrument_name"
          ),
        character(1)
      ),
      collection_method = vapply(
        instrument_records,
        function(record)
          glc_plan_metadata_character(
            record$instrument,
            "collection_method"
          ),
        character(1)
      ),
      recorded_by = vapply(
        instrument_records,
        function(record)
          glc_plan_metadata_character(
            record$instrument,
            "recorded_by"
          ),
        character(1)
      ),
      software_name = vapply(
        instrument_records,
        function(record)
          glc_plan_metadata_character(
            record$instrument,
            "software_name"
          ),
        character(1)
      )
    )
  }

  variable_rows <- unlist(
    lapply(records, function(record) {
      selected_names <- vapply(
        record$selected_variables,
        function(variable) variable$name,
        character(1)
      )
      lapply(seq_along(record$variables), function(position) {
        variable <- record$variables[[position]]
        list(
          record = record,
          variable = variable,
          position = as.integer(position),
          selected = variable$name %in% selected_names
        )
      })
    }),
    recursive = FALSE
  )
  variable_table <- if (length(variable_rows) == 0L) {
    glc_plan_empty_file_group_variables()
  } else {
    tibble::tibble(
      dataset_id = vapply(
        variable_rows,
        function(row) row$record$dataset_id,
        character(1)
      ),
      file_group_id = vapply(
        variable_rows,
        function(row) row$record$file_group_id,
        character(1)
      ),
      declaration_position = vapply(
        variable_rows,
        function(row) row$position,
        integer(1)
      ),
      selected_by_request = vapply(
        variable_rows,
        function(row) row$selected,
        logical(1)
      ),
      selected_for_output = vapply(
        variable_rows,
        function(row) row$selected,
        logical(1)
      ),
      selection_origin = vapply(
        variable_rows,
        function(row) if (row$selected) selection_origin else NA_character_,
        character(1)
      ),
      name = vapply(
        variable_rows,
        function(row) row$variable$name,
        character(1)
      ),
      label = vapply(
        variable_rows,
        function(row) row$variable$label,
        character(1)
      ),
      description = vapply(
        variable_rows,
        function(row) row$variable$description,
        character(1)
      ),
      unit = vapply(
        variable_rows,
        function(row) row$variable$unit,
        character(1)
      ),
      calibration = vapply(
        variable_rows,
        function(row) row$variable$calibration,
        character(1)
      ),
      type = vapply(
        variable_rows,
        function(row) row$variable$type,
        character(1)
      ),
      term = vapply(
        variable_rows,
        function(row) row$variable$term,
        character(1)
      ),
      term_name = vapply(
        variable_rows,
        function(row) row$variable$term_name,
        character(1)
      ),
      primary = vapply(
        variable_rows,
        function(row) row$variable$primary,
        logical(1)
      ),
      factor_level_count = vapply(
        variable_rows,
        function(row) length(row$variable$factor_levels),
        integer(1)
      )
    )
  }

  level_rows <- unlist(
    lapply(variable_rows, function(row) {
      lapply(seq_along(row$variable$factor_levels), function(level_position) {
        list(
          record = row$record,
          variable = row$variable,
          variable_position = row$position,
          level_position = as.integer(level_position),
          level = row$variable$factor_levels[[level_position]]
        )
      })
    }),
    recursive = FALSE
  )
  level_table <- if (length(level_rows) == 0L) {
    glc_plan_empty_file_group_factor_levels()
  } else {
    tibble::tibble(
      dataset_id = vapply(
        level_rows,
        function(row) row$record$dataset_id,
        character(1)
      ),
      file_group_id = vapply(
        level_rows,
        function(row) row$record$file_group_id,
        character(1)
      ),
      variable_position = vapply(
        level_rows,
        function(row) row$variable_position,
        integer(1)
      ),
      variable_name = vapply(
        level_rows,
        function(row) row$variable$name,
        character(1)
      ),
      level_position = vapply(
        level_rows,
        function(row) row$level_position,
        integer(1)
      ),
      value = vapply(
        level_rows,
        function(row) row$level$value,
        character(1)
      ),
      label = vapply(
        level_rows,
        function(row) row$level$label,
        character(1)
      ),
      description = vapply(
        level_rows,
        function(row) row$level$description,
        character(1)
      )
    )
  }
  list(
    instruments = glc_plan_metadata_order(
      instrument_table,
      c("dataset_id", "file_group_id")
    ),
    file_group_variables = glc_plan_metadata_order(
      variable_table,
      c("dataset_id", "file_group_id", "declaration_position")
    ),
    file_group_factor_levels = glc_plan_metadata_order(
      level_table,
      c(
        "dataset_id",
        "file_group_id",
        "variable_position",
        "variable_name",
        "level_position"
      )
    )
  )
}

glc_plan_empty_metadata_extensions <- function() {
  tibble::tibble(
    resource = character(),
    entity_type = character(),
    entity_id = character(),
    parent_id = character(),
    position = integer(),
    metadata = list()
  )
}

glc_plan_metadata_extension_rows <- function(
  records,
  resource,
  entity_type,
  id_aliases,
  known
) {
  rows <- list()
  for (position in seq_along(records)) {
    record <- records[[position]]
    fields <- glc_plan_unknown_fields(record, known)
    if (length(fields) == 0L) {
      next
    }
    rows[[length(rows) + 1L]] <- tibble::tibble(
      resource = resource,
      entity_type = entity_type,
      entity_id = glc_plan_metadata_character(record, id_aliases),
      parent_id = NA_character_,
      position = as.integer(position),
      metadata = list(fields)
    )
  }
  rows
}

glc_plan_metadata_extensions <- function(loaded, model) {
  specifications <- list(
    list(
      records = loaded$records$study,
      resource = "study",
      entity_type = "study",
      id_aliases = c("study_internal_id", "study_id"),
      known = c(
        "schema_version",
        "study_internal_id",
        "study_id",
        "study_title",
        "title",
        "study_short_description",
        "short_description",
        "study_preregistration",
        "preregistration",
        "study_registration",
        "registration",
        "study_ethics",
        "ethics",
        "study_sample",
        "sample",
        "study_intervention",
        "intervention",
        "study_groups",
        "groups",
        "study_setting",
        "setting",
        "study_geographical_location",
        "geographical_location",
        "study_contributors",
        "contributors",
        "study_datasets",
        "dataset_ids",
        "study_type",
        "type",
        "study_funding_sources",
        "funding_sources",
        "study_keywords",
        "keywords"
      )
    ),
    list(
      records = loaded$records$participants,
      resource = "participants",
      entity_type = "participant",
      id_aliases = c("participant_internal_id", "participant_id"),
      known = c(
        "participant_internal_id",
        "participant_id",
        "participant_age",
        "age",
        "participant_sex",
        "sex",
        "participant_gender",
        "gender"
      )
    ),
    list(
      records = loaded$records$participant_characteristics,
      resource = "participant_characteristics",
      entity_type = "participant_characteristic",
      id_aliases = c(
        "participant_internal_id",
        "participant_id",
        "participant_characteristic_participant_id",
        "participant_characteristic_crossref_participant_id"
      ),
      known = c(
        "participant_internal_id",
        "participant_id",
        "participant_characteristic_participant_id",
        "participant_characteristic_crossref_participant_id",
        "participant_characteristic_name",
        "characteristic_name",
        "participant_characteristic_value",
        "participant_characteristic_values",
        "characteristic_value",
        "participant_characteristic_unit",
        "characteristic_unit",
        "participant_characteristic_description",
        "characteristic_description"
      )
    ),
    list(
      records = loaded$records$devices,
      resource = "devices",
      entity_type = "device",
      id_aliases = c("device_internal_id", "device_id"),
      known = c(
        "schema_version",
        "device_internal_id",
        "device_id",
        "device_manufacturer",
        "manufacturer",
        "device_model",
        "model",
        "device_serial_number",
        "serial_number",
        "device_calibration_date",
        "calibration_date",
        "device_firmware_version",
        "firmware_version",
        "device_datasheet_id",
        "datasheet_id",
        "device_sensors",
        "sensors"
      )
    ),
    list(
      records = loaded$records$device_datasheets,
      resource = "device_datasheets",
      entity_type = "datasheet",
      id_aliases = c("datasheet_id", "device_datasheet_internal_id"),
      known = c(
        "schema_version",
        "datasheet_id",
        "device_datasheet_internal_id",
        "datasheet_version",
        "datasheet_manufacturer",
        "manufacturer",
        "datasheet_type",
        "type",
        "datasheet_sensor_modality",
        "sensor_modality",
        "modalities",
        "datasheet_sensor_modality_other",
        "modality_other",
        "datasheet_model",
        "model",
        "datasheet_calibration_interval",
        "datasheet_calibration_method",
        "calibration_method",
        "datasheet_calibration_accuracy",
        "calibration_accuracy",
        "datasheet_calibration_range",
        "calibration_range",
        "datasheet_calibration_notes",
        "calibration_notes",
        "datasheet_calibration_parameters",
        "calibration_parameters",
        "datasheet_calibration_spectral_sensitivity",
        "datasheet_calibration_linearity",
        "datasheet_calibration_directional_response",
        "datasheet_channel",
        "channels"
      )
    ),
    list(
      records = loaded$records$contributors,
      resource = "contributors",
      entity_type = "contributor",
      id_aliases = c("contributor_internal_id", "contributor_id"),
      known = c(
        "contributor_internal_id",
        "contributor_id",
        "contributor_full_name",
        "full_name",
        "name",
        "contributor_roles",
        "roles",
        "contributor_email",
        "email",
        "contributor_orcid",
        "orcid",
        "contributor_institution",
        "institution"
      )
    ),
    list(
      records = lapply(model$datasets, function(dataset) dataset$raw),
      resource = "datasets",
      entity_type = "dataset",
      id_aliases = "dataset_internal_id",
      known = c(
        "schema_version",
        "dataset_internal_id",
        "dataset_participant_associated",
        "dataset_crossref",
        "dataset_timezone",
        "dataset_location",
        "dataset_variable_terms",
        "dataset_file",
        "dataset_sampling_interval",
        "dataset_datetime",
        "dataset_device_location",
        "dataset_instructions"
      )
    )
  )
  rows <- unlist(
    lapply(specifications, function(specification) {
      glc_plan_metadata_extension_rows(
        specification$records,
        specification$resource,
        specification$entity_type,
        specification$id_aliases,
        specification$known
      )
    }),
    recursive = FALSE
  )
  if (length(rows) == 0L) {
    return(glc_plan_empty_metadata_extensions())
  }
  glc_plan_metadata_order(
    dplyr::bind_rows(rows),
    c("resource", "entity_type", "entity_id", "position")
  )
}

glc_plan_metadata_snapshot <- function(x, model, records, variable_scope) {
  loaded <- glc_plan_metadata_load_resources(x)
  studies <- glc_plan_metadata_studies(loaded$records$study)
  datasets <- glc_plan_metadata_datasets(model)
  devices <- glc_plan_metadata_devices(loaded$records$devices)
  datasheets <- glc_plan_metadata_datasheets(
    loaded$records$device_datasheets
  )
  metadata <- c(
    list(
      schema = glc_collection_metadata_schema(),
      version = glc_collection_metadata_version(),
      resource_status = glc_plan_metadata_resource_status(
        loaded,
        length(model$datasets)
      )
    ),
    studies,
    list(
      contributors = glc_plan_metadata_contributors(
        loaded$records$contributors
      )
    ),
    datasets,
    list(
      participants = glc_plan_metadata_participants(
        loaded$records$participants
      ),
      participant_characteristics = glc_plan_metadata_participant_characteristics(
        loaded$records$participant_characteristics
      )
    ),
    devices,
    datasheets
  )
  linked <- glc_plan_metadata_apply_links(metadata, records)
  metadata <- linked$metadata
  records <- linked$records
  group_tables <- glc_plan_metadata_group_tables(records, variable_scope)
  metadata <- c(
    metadata,
    group_tables,
    list(extensions = glc_plan_metadata_extensions(loaded, model))
  )
  list(metadata = metadata, records = records)
}
