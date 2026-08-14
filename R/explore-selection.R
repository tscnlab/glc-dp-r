glc_explorer_nonempty_values <- function(x) {
  value <- unique(as.character(x %||% character()))
  value[!is.na(value) & nzchar(value)]
}

glc_explorer_metadata_resource_leaves <- function(metadata, resource) {
  value <- metadata[[resource]]
  if (is.null(value)) {
    return(tibble::tibble(
      resource = character(),
      record = integer(),
      field = character(),
      value = character(),
      context = character()
    ))
  }
  glc_metadata_leaf_table(stats::setNames(list(value), resource))
}

glc_explorer_metadata_field_values <- function(metadata, aliases) {
  leaf <- tolower(sub("^.*\\.", "", metadata$field))
  glc_explorer_nonempty_values(metadata$value[leaf %in% aliases])
}

glc_explorer_metadata_scalar_records <- function(
  metadata,
  resource,
  fields
) {
  leaves <- glc_explorer_metadata_resource_leaves(metadata, resource)
  if (nrow(leaves) == 0L) {
    result <- stats::setNames(
      rep(list(character()), length(fields)),
      names(fields)
    )
    return(tibble::as_tibble(result))
  }
  record <- ifelse(
    is.na(leaves$record),
    "object",
    paste0("record:", leaves$record)
  )
  keys <- unique(record)
  result <- lapply(fields, function(aliases) {
    vapply(
      keys,
      function(key) {
        values <- glc_explorer_metadata_field_values(
          leaves[record == key, , drop = FALSE],
          aliases
        )
        if (length(values) == 0L) NA_character_ else values[[1L]]
      },
      character(1)
    )
  })
  tibble::as_tibble(stats::setNames(result, names(fields)))
}

glc_explorer_collapse_metadata_records <- function(records, id_column) {
  if (nrow(records) == 0L) {
    return(records)
  }
  ids <- as.character(records[[id_column]])
  keep <- !is.na(ids) & nzchar(ids)
  records <- records[keep, , drop = FALSE]
  ids <- ids[keep]
  unique_ids <- unique(ids)
  result <- lapply(unique_ids, function(id) {
    rows <- records[ids == id, , drop = FALSE]
    values <- lapply(rows, function(column) {
      present <- glc_explorer_nonempty_values(column)
      if (length(present) == 0L) NA_character_ else present[[1L]]
    })
    tibble::as_tibble(values)
  })
  dplyr::bind_rows(result)
}

glc_explorer_normalize_participants <- function(metadata) {
  records <- glc_explorer_metadata_scalar_records(
    metadata,
    "participants",
    list(
      participant_id = c("participant_internal_id", "participant_id"),
      age = c("participant_age", "age"),
      sex = c("participant_sex", "sex"),
      gender = c("participant_gender", "gender")
    )
  )
  glc_explorer_collapse_metadata_records(records, "participant_id")
}

glc_explorer_characteristics_from_resource <- function(metadata, resource) {
  leaves <- glc_explorer_metadata_resource_leaves(metadata, resource)
  if (nrow(leaves) == 0L) {
    return(tibble::tibble(
      participant_id = character(),
      characteristic_name = character(),
      characteristic_value = character()
    ))
  }
  record <- ifelse(
    is.na(leaves$record),
    "object",
    paste0("record:", leaves$record)
  )
  aliases <- list(
    participant_id = c(
      "participant_internal_id",
      "participant_id",
      "participant_characteristic_participant_id",
      "participant_characteristics_participant_id",
      "participant_characteristic_crossref_participant_id",
      "participant_characteristics_crossref_participant_id"
    ),
    characteristic_name = c(
      "participant_characteristic_name",
      "participant_characteristics_name",
      "characteristic_name"
    ),
    characteristic_value = c(
      "participant_characteristic_value",
      "participant_characteristics_value",
      "participant_characteristic_values",
      "participant_characteristics_values",
      "characteristic_value"
    )
  )
  rows <- lapply(unique(record), function(key) {
    values <- leaves[record == key, , drop = FALSE]
    participant_id <- glc_explorer_metadata_field_values(
      values,
      aliases$participant_id
    )
    names <- glc_explorer_metadata_field_values(
      values,
      aliases$characteristic_name
    )
    characteristic_values <- glc_explorer_metadata_field_values(
      values,
      aliases$characteristic_value
    )
    if (
      length(participant_id) == 0L ||
        length(names) == 0L ||
        length(characteristic_values) == 0L
    ) {
      return(NULL)
    }
    combinations <- expand.grid(
      characteristic_name = names,
      characteristic_value = characteristic_values,
      stringsAsFactors = FALSE
    )
    tibble::tibble(
      participant_id = participant_id[[1L]],
      characteristic_name = combinations$characteristic_name,
      characteristic_value = combinations$characteristic_value
    )
  })
  result <- dplyr::bind_rows(rows)
  if (nrow(result) == 0L) {
    return(tibble::tibble(
      participant_id = character(),
      characteristic_name = character(),
      characteristic_value = character()
    ))
  }
  dplyr::distinct(result)
}

glc_explorer_normalize_participant_characteristics <- function(metadata) {
  dplyr::bind_rows(
    glc_explorer_characteristics_from_resource(
      metadata,
      "participant_characteristics"
    ),
    glc_explorer_characteristics_from_resource(metadata, "participants")
  ) |>
    dplyr::distinct()
}

glc_explorer_normalize_devices <- function(metadata) {
  records <- glc_explorer_metadata_scalar_records(
    metadata,
    "devices",
    list(
      device_id = c("device_internal_id", "device_id"),
      manufacturer = c(
        "device_manufacturer",
        "manufacturer",
        "datasheet_manufacturer"
      ),
      model = c("device_model", "model", "datasheet_model"),
      sensor_type = c(
        "device_sensor_type",
        "sensor_type",
        "device_type",
        "datasheet_type"
      )
    )
  )
  glc_explorer_collapse_metadata_records(records, "device_id")
}

glc_explorer_normalize_selection_metadata <- function(metadata) {
  list(
    participants = glc_explorer_normalize_participants(metadata),
    participant_characteristics = glc_explorer_normalize_participant_characteristics(
      metadata
    ),
    devices = glc_explorer_normalize_devices(metadata)
  )
}

glc_explorer_group_inventory <- function(package) {
  datasets <- glc_model(package)$datasets
  rows <- list()
  row_index <- 0L
  for (dataset in datasets) {
    for (group in dataset$groups) {
      row_index <- row_index + 1L
      variables <- tibble::tibble(
        name = vapply(
          group$variables,
          function(variable) variable$name,
          character(1)
        ),
        label = vapply(
          group$variables,
          function(variable) variable$label,
          character(1)
        ),
        description = vapply(
          group$variables,
          function(variable) variable$description,
          character(1)
        ),
        unit = vapply(
          group$variables,
          function(variable) variable$unit,
          character(1)
        ),
        type = vapply(
          group$variables,
          function(variable) variable$type,
          character(1)
        ),
        term = vapply(
          group$variables,
          function(variable) variable$term,
          character(1)
        ),
        term_name = vapply(
          group$variables,
          function(variable) variable$term_name,
          character(1)
        ),
        factor_values = lapply(group$variables, function(variable) {
          vapply(
            variable$factor_levels,
            function(level) level$value,
            character(1)
          )
        }),
        factor_labels = lapply(group$variables, function(variable) {
          vapply(
            variable$factor_levels,
            function(level) level$label,
            character(1)
          )
        }),
        factor_descriptions = lapply(group$variables, function(variable) {
          vapply(
            variable$factor_levels,
            function(level) level$description,
            character(1)
          )
        }),
        primary = vapply(
          group$variables,
          function(variable) variable$primary,
          logical(1)
        )
      )
      rows[[row_index]] <- tibble::tibble(
        dataset_id = dataset$id,
        file_group = group$index,
        file_group_id = group$id,
        device_id = group$device_id,
        device_location = group$device_location,
        device_location_type = group$device_location_type,
        format = group$format,
        timezone = group$timezone,
        modalities = list(group$modality),
        modality_other = group$modality_other,
        modality_other_type = group$modality_other_type,
        role = group$role,
        data_state = group$data_state,
        temporal_type = group$temporal_type,
        temporal_value = group$temporal_value,
        temporal_unit = group$temporal_unit,
        datetime_source = group$datetime$source,
        datetime_date = group$datetime$date,
        datetime_format = group$datetime$date_format,
        datetime_time = group$datetime$time,
        datetime_time_format = group$datetime$time_format,
        file_count = length(group$files),
        variable_names = list(glc_explorer_nonempty_values(variables$name)),
        variable_terms = list(glc_explorer_nonempty_values(variables$term)),
        variables = list(variables)
      )
    }
  }
  if (length(rows) > 0L) {
    return(dplyr::bind_rows(rows))
  }
  tibble::tibble(
    dataset_id = character(),
    file_group = integer(),
    file_group_id = character(),
    device_id = character(),
    device_location = character(),
    device_location_type = character(),
    format = character(),
    timezone = character(),
    modalities = list(),
    modality_other = character(),
    modality_other_type = character(),
    role = character(),
    data_state = character(),
    temporal_type = character(),
    temporal_value = numeric(),
    temporal_unit = character(),
    datetime_source = character(),
    datetime_date = character(),
    datetime_format = character(),
    datetime_time = character(),
    datetime_time_format = character(),
    file_count = integer(),
    variable_names = list(),
    variable_terms = list(),
    variables = list()
  )
}

glc_explorer_load_selection <- function(package) {
  available_resources <- unique(glc_resources(package)$resource)
  requested <- intersect(
    c("participants", "participant_characteristics", "devices"),
    available_resources
  )
  metadata <- list()
  metadata_issues <- character()
  for (resource in requested) {
    value <- tryCatch(
      glc_metadata(package, resources = resource)[[resource]],
      error = identity
    )
    if (inherits(value, "error")) {
      metadata_issues <- c(
        metadata_issues,
        paste0(resource, ": ", conditionMessage(value))
      )
    } else {
      metadata[[resource]] <- value
    }
  }
  normalized <- glc_explorer_normalize_selection_metadata(metadata)
  c(
    normalized,
    list(
      datasets = glc_datasets(package),
      files = glc_files(package),
      variables = glc_variables(package),
      groups = glc_explorer_group_inventory(package),
      metadata_issues = metadata_issues
    )
  )
}

glc_explorer_facet_is_active <- function(value) {
  length(glc_explorer_nonempty_values(value)) > 0L
}

glc_explorer_numeric_age_range <- function(value) {
  ages <- suppressWarnings(as.numeric(value %||% numeric()))
  ages <- ages[is.finite(ages)]
  if (length(ages) == 0L) {
    return(numeric())
  }
  range(ages)
}

glc_explorer_numeric_slider_spec <- function(value) {
  numbers <- suppressWarnings(as.numeric(value %||% numeric()))
  numbers <- numbers[is.finite(numbers)]
  if (length(numbers) == 0L) {
    return(NULL)
  }
  bounds <- range(numbers)
  whole_numbers <- all(
    abs(numbers - round(numbers)) < sqrt(.Machine$double.eps)
  )
  step <- if (whole_numbers) 1 else NULL
  if (identical(bounds[[1L]], bounds[[2L]])) {
    bounds[[2L]] <- bounds[[2L]] + if (whole_numbers) 1 else 0.1
  }
  list(
    min = bounds[[1L]],
    max = bounds[[2L]],
    value = bounds,
    step = step
  )
}

glc_explorer_age_slider_spec <- function(value) {
  glc_explorer_numeric_slider_spec(value)
}

glc_explorer_age_filter_value <- function(value, available_range) {
  selected <- glc_explorer_numeric_age_range(value)
  available <- glc_explorer_numeric_age_range(available_range)
  if (length(selected) == 0L || length(available) == 0L) {
    return(numeric())
  }
  if (isTRUE(all.equal(selected, available, tolerance = 1e-7))) {
    return(numeric())
  }
  selected
}

glc_explorer_characteristic_filter_spec <- function(
  characteristics,
  name
) {
  name <- glc_explorer_nonempty_values(name)
  if (
    length(name) != 1L ||
      !inherits(characteristics, "data.frame") ||
      nrow(characteristics) == 0L
  ) {
    return(list(
      type = "categorical",
      name = "",
      values = character(),
      slider = NULL
    ))
  }
  values <- glc_explorer_nonempty_values(
    characteristics$characteristic_value[
      characteristics$characteristic_name == name
    ]
  )
  numbers <- suppressWarnings(as.numeric(values))
  numeric <- length(values) > 0L && all(is.finite(numbers))
  list(
    type = if (numeric) "numeric" else "categorical",
    name = name[[1L]],
    values = values,
    slider = if (numeric) glc_explorer_numeric_slider_spec(numbers) else NULL
  )
}

glc_explorer_characteristic_filter_value <- function(value, spec) {
  if (!identical(spec$type, "numeric") || is.null(spec$slider)) {
    return(value %||% character())
  }
  glc_explorer_age_filter_value(value, spec$slider$value)
}

glc_explorer_participant_facets_active <- function(facets) {
  glc_explorer_facet_is_active(facets$age) ||
    glc_explorer_facet_is_active(facets$sex) ||
    glc_explorer_facet_is_active(facets$gender) ||
    (glc_explorer_facet_is_active(facets$characteristic_name) &&
      glc_explorer_facet_is_active(facets$characteristic_values))
}

glc_explorer_device_facets_active <- function(facets) {
  glc_explorer_facet_is_active(facets$manufacturer) ||
    glc_explorer_facet_is_active(facets$model) ||
    glc_explorer_facet_is_active(facets$sensor_type)
}

glc_explorer_filter_participant_ids <- function(
  participants,
  characteristics,
  facets
) {
  if (nrow(participants) == 0L) {
    return(character())
  }
  keep <- rep(TRUE, nrow(participants))
  age_range <- glc_explorer_numeric_age_range(facets$age)
  if (length(age_range) > 0L) {
    age <- suppressWarnings(as.numeric(participants$age))
    keep <- keep &
      is.finite(age) &
      age >= age_range[[1L]] &
      age <= age_range[[2L]]
  }
  for (field in c("sex", "gender")) {
    selected <- glc_explorer_nonempty_values(facets[[field]])
    if (length(selected) > 0L) {
      value <- as.character(participants[[field]])
      keep <- keep & !is.na(value) & nzchar(value) & value %in% selected
    }
  }
  characteristic_name <- glc_explorer_nonempty_values(
    facets$characteristic_name
  )
  characteristic_values <- glc_explorer_nonempty_values(
    facets$characteristic_values
  )
  if (
    length(characteristic_name) > 0L &&
      length(characteristic_values) > 0L
  ) {
    characteristic_rows <- characteristics$characteristic_name %in%
      characteristic_name
    characteristic_spec <- glc_explorer_characteristic_filter_spec(
      characteristics,
      characteristic_name
    )
    if (identical(characteristic_spec$type, "numeric")) {
      selected_range <- glc_explorer_numeric_age_range(
        characteristic_values
      )
      if (length(selected_range) == 0L) {
        characteristic_rows[] <- FALSE
      } else {
        values <- suppressWarnings(as.numeric(
          characteristics$characteristic_value
        ))
        characteristic_rows <- characteristic_rows &
          is.finite(values) &
          values >= selected_range[[1L]] &
          values <= selected_range[[2L]]
      }
    } else {
      characteristic_rows <- characteristic_rows &
        characteristics$characteristic_value %in% characteristic_values
    }
    matched <- characteristics$participant_id[characteristic_rows]
    keep <- keep & participants$participant_id %in% matched
  }
  unique(participants$participant_id[keep])
}

glc_explorer_filter_device_ids <- function(devices, facets) {
  if (nrow(devices) == 0L) {
    return(character())
  }
  keep <- rep(TRUE, nrow(devices))
  for (field in c("manufacturer", "model", "sensor_type")) {
    selected <- glc_explorer_nonempty_values(facets[[field]])
    if (length(selected) > 0L) {
      value <- as.character(devices[[field]])
      keep <- keep & !is.na(value) & nzchar(value) & value %in% selected
    }
  }
  unique(devices$device_id[keep])
}

glc_explorer_narrow_ids <- function(eligible, selected, facet_active) {
  selected <- glc_explorer_nonempty_values(selected)
  if (length(selected) > 0L) {
    return(list(
      ids = intersect(eligible, selected),
      restricted = TRUE
    ))
  }
  list(ids = eligible, restricted = isTRUE(facet_active))
}

glc_explorer_selection_scope <- function(
  selection,
  facets,
  participant_ids = character(),
  device_ids = character()
) {
  eligible_participants <- glc_explorer_filter_participant_ids(
    selection$participants,
    selection$participant_characteristics,
    facets$participant
  )
  eligible_devices <- glc_explorer_filter_device_ids(
    selection$devices,
    facets$device
  )
  participant_scope <- glc_explorer_narrow_ids(
    eligible_participants,
    participant_ids,
    glc_explorer_participant_facets_active(facets$participant)
  )
  device_scope <- glc_explorer_narrow_ids(
    eligible_devices,
    device_ids,
    glc_explorer_device_facets_active(facets$device)
  )

  datasets <- selection$datasets
  keep <- rep(TRUE, nrow(datasets))
  if (participant_scope$restricted) {
    participant_id <- as.character(datasets$participant_id)
    keep <- keep &
      !is.na(participant_id) &
      nzchar(participant_id) &
      participant_id %in% participant_scope$ids
  }
  if (device_scope$restricted) {
    device_dataset_ids <- selection$groups$dataset_id[
      !is.na(selection$groups$device_id) &
        selection$groups$device_id %in% device_scope$ids
    ]
    keep <- keep & datasets$dataset_id %in% device_dataset_ids
  }
  list(
    eligible_participant_ids = eligible_participants,
    participant_ids = participant_scope$ids,
    participant_restricted = participant_scope$restricted,
    eligible_device_ids = eligible_devices,
    device_ids = device_scope$ids,
    device_restricted = device_scope$restricted,
    dataset_ids = datasets$dataset_id[keep]
  )
}

glc_explorer_selected_variable_rows <- function(
  variables,
  variable_names = character(),
  variable_terms = character()
) {
  variable_names <- glc_explorer_nonempty_values(variable_names)
  variable_terms <- glc_explorer_nonempty_values(variable_terms)
  if (!inherits(variables, "data.frame") || nrow(variables) == 0L) {
    return(variables)
  }
  if (length(variable_names) > 0L) {
    selected <- variables$name %in% variable_names
  } else if (length(variable_terms) > 0L) {
    terms <- if ("term" %in% names(variables)) {
      variables$term
    } else {
      rep(NA_character_, nrow(variables))
    }
    selected <- terms %in% variable_terms
  } else {
    selected <- rep(TRUE, nrow(variables))
  }
  variables[selected, , drop = FALSE]
}

glc_explorer_variable_value <- function(
  variables,
  field,
  index,
  default = NA_character_
) {
  if (!field %in% names(variables)) {
    return(default)
  }
  value <- variables[[field]][[index]]
  if (length(value) == 0L) {
    return(default)
  }
  value[[1L]]
}

glc_explorer_factor_levels <- function(variables, index) {
  factor_values <- if ("factor_values" %in% names(variables)) {
    as.character(variables$factor_values[[index]])
  } else {
    character()
  }
  factor_labels <- if ("factor_labels" %in% names(variables)) {
    as.character(variables$factor_labels[[index]])
  } else {
    character()
  }
  factor_descriptions <- if ("factor_descriptions" %in% names(variables)) {
    as.character(variables$factor_descriptions[[index]])
  } else {
    character()
  }
  count <- max(
    length(factor_values),
    length(factor_labels),
    length(factor_descriptions)
  )
  if (count == 0L) {
    return(list())
  }
  value_at <- function(values, position) {
    if (length(values) < position) NA_character_ else values[[position]]
  }
  lapply(seq_len(count), function(position) {
    list(
      value = value_at(factor_values, position),
      label = value_at(factor_labels, position),
      description = value_at(factor_descriptions, position)
    )
  })
}

glc_explorer_declared_variables <- function(variables) {
  if (!inherits(variables, "data.frame") || nrow(variables) == 0L) {
    return(list())
  }
  lapply(seq_len(nrow(variables)), function(index) {
    list(
      name = glc_scalar_character(
        glc_explorer_variable_value(variables, "name", index)
      ),
      label = glc_scalar_character(
        glc_explorer_variable_value(variables, "label", index)
      ),
      description = glc_scalar_character(
        glc_explorer_variable_value(variables, "description", index)
      ),
      unit = glc_scalar_character(
        glc_explorer_variable_value(variables, "unit", index)
      ),
      calibration = glc_scalar_character(
        glc_explorer_variable_value(variables, "calibration", index)
      ),
      type = glc_scalar_character(
        glc_explorer_variable_value(variables, "type", index)
      ),
      term = glc_scalar_character(
        glc_explorer_variable_value(variables, "term", index)
      ),
      term_name = glc_scalar_character(
        glc_explorer_variable_value(variables, "term_name", index)
      ),
      primary = glc_scalar_logical(
        glc_explorer_variable_value(variables, "primary", index, FALSE),
        FALSE
      ),
      factor_levels = glc_explorer_factor_levels(variables, index)
    )
  })
}

glc_explorer_group_scalar <- function(
  groups,
  field,
  index,
  default = NA_character_
) {
  if (!field %in% names(groups)) {
    return(default)
  }
  value <- groups[[field]][[index]]
  if (length(value) == 0L) {
    return(default)
  }
  value[[1L]]
}

glc_explorer_declared_records <- function(groups) {
  if (nrow(groups) == 0L) {
    return(list())
  }
  records <- lapply(seq_len(nrow(groups)), function(index) {
    file_count <- suppressWarnings(as.integer(glc_explorer_group_scalar(
      groups,
      "file_count",
      index,
      NA_integer_
    )))
    files_known <- length(file_count) == 1L && !is.na(file_count)
    if (!files_known) {
      file_count <- 1L
    }
    files <- lapply(seq_len(max(0L, file_count)), function(file_index) {
      list(
        declared_path = NA_character_,
        encoding = NA_character_,
        declared_bytes = NA_real_
      )
    })
    variables <- glc_explorer_declared_variables(groups$variables[[index]])
    list(
      dataset_id = glc_scalar_character(
        glc_explorer_group_scalar(groups, "dataset_id", index)
      ),
      dataset_schema_version = NA_character_,
      study_id = NA_character_,
      participant_id = NA_character_,
      participant_associated = NA,
      file_group = as.integer(glc_explorer_group_scalar(
        groups,
        "file_group",
        index,
        index
      )),
      file_group_id = glc_scalar_character(
        glc_explorer_group_scalar(groups, "file_group_id", index)
      ),
      description = NA_character_,
      device_id = glc_scalar_character(
        glc_explorer_group_scalar(groups, "device_id", index)
      ),
      device_location = glc_scalar_character(
        glc_explorer_group_scalar(groups, "device_location", index)
      ),
      device_location_type = glc_scalar_character(
        glc_explorer_group_scalar(groups, "device_location_type", index)
      ),
      format = glc_scalar_character(
        glc_explorer_group_scalar(groups, "format", index)
      ),
      timezone = glc_scalar_character(
        glc_explorer_group_scalar(groups, "timezone", index)
      ),
      modalities = if ("modalities" %in% names(groups)) {
        as.character(groups$modalities[[index]])
      } else {
        character()
      },
      modality_other = glc_scalar_character(
        glc_explorer_group_scalar(groups, "modality_other", index)
      ),
      modality_other_type = glc_scalar_character(
        glc_explorer_group_scalar(groups, "modality_other_type", index)
      ),
      role = glc_scalar_character(
        glc_explorer_group_scalar(groups, "role", index)
      ),
      data_state = glc_scalar_character(
        glc_explorer_group_scalar(groups, "data_state", index)
      ),
      temporal_type = glc_scalar_character(
        glc_explorer_group_scalar(groups, "temporal_type", index)
      ),
      temporal_value = suppressWarnings(as.numeric(glc_explorer_group_scalar(
        groups,
        "temporal_value",
        index,
        NA_real_
      ))),
      temporal_unit = glc_scalar_character(
        glc_explorer_group_scalar(groups, "temporal_unit", index)
      ),
      header_row = NA_integer_,
      preprocessing = character(),
      datetime = list(
        source = glc_scalar_character(
          glc_explorer_group_scalar(groups, "datetime_source", index)
        ),
        date = glc_scalar_character(
          glc_explorer_group_scalar(groups, "datetime_date", index)
        ),
        date_format = glc_scalar_character(
          glc_explorer_group_scalar(groups, "datetime_format", index)
        ),
        time = glc_scalar_character(
          glc_explorer_group_scalar(groups, "datetime_time", index)
        ),
        time_format = glc_scalar_character(
          glc_explorer_group_scalar(groups, "datetime_time_format", index)
        )
      ),
      variables = variables,
      files = files,
      file_declarations_known = files_known,
      extensions = list()
    )
  })
  ids <- vapply(records, function(record) record$file_group_id, character(1))
  records[order(glc_plan_utf8_key(ids), method = "radix")]
}

glc_explorer_planner_restrictions <- function(
  requested_dataset_ids,
  candidate_groups,
  participant_restricted = FALSE,
  device_restricted = FALSE,
  group_filter_active = FALSE,
  requested_file_group_ids = character()
) {
  dataset_id <- glc_plan_sort_utf8(
    glc_explorer_nonempty_values(requested_dataset_ids)
  )
  requested_file_group_ids <- glc_plan_sort_utf8(
    glc_explorer_nonempty_values(requested_file_group_ids)
  )
  translated_filter <- isTRUE(participant_restricted) ||
    isTRUE(device_restricted) ||
    isTRUE(group_filter_active)
  if (translated_filter) {
    file_group <- glc_plan_sort_utf8(
      glc_explorer_nonempty_values(candidate_groups$file_group_id)
    )
    file_group_basis <- "translated_candidate_universe"
  } else if (length(requested_file_group_ids) > 0L) {
    file_group <- requested_file_group_ids
    file_group_basis <- "explicit_file_group"
  } else {
    file_group <- character()
    file_group_basis <- "omitted"
  }
  list(
    dataset_id = dataset_id,
    file_group = file_group,
    dataset_id_explicit = length(dataset_id) > 0L,
    file_group_basis = file_group_basis
  )
}

glc_explorer_declared_request <- function(
  variable_names = character(),
  variable_terms = character(),
  dataset_id = character(),
  file_group = character(),
  standardize = "lightlogr"
) {
  variable_names <- glc_explorer_nonempty_values(variable_names)
  variable_terms <- glc_explorer_nonempty_values(variable_terms)
  variable_scope <- if (length(variable_names) > 0L) {
    "selected"
  } else if (length(variable_terms) > 0L) {
    "matched"
  } else {
    "all"
  }
  list(
    terms = glc_plan_sort_utf8(variable_terms),
    term_match = "all",
    term_identifier = "canonical",
    labels_used_for_matching = FALSE,
    variable_scope = variable_scope,
    requested_variables = glc_plan_sort_utf8(variable_names),
    dataset_id = glc_plan_sort_utf8(
      glc_explorer_nonempty_values(dataset_id)
    ),
    file_group = glc_plan_sort_utf8(
      glc_explorer_nonempty_values(file_group)
    ),
    standardize = match.arg(standardize, c("lightlogr", "none"))
  )
}

glc_explorer_declared_provenance <- function(package = NULL) {
  package_id <- if (is.null(package)) NA_character_ else
    glc_plan_package_id(package)
  repository <- if (is.null(package)) {
    NA_character_
  } else {
    glc_scalar_character(package$repo)
  }
  source_revision <- if (is.null(package)) {
    NA_character_
  } else {
    glc_scalar_character(package$commit)
  }
  schema_version <- if (is.null(package)) {
    NA_character_
  } else {
    glc_scalar_character(package$schema_version)
  }
  if (is.na(package_id) || !nzchar(package_id)) {
    package_id <- "glc-explorer"
  }
  if (is.na(repository) || !nzchar(repository)) {
    repository <- "glc-explorer"
  }
  if (
    is.na(source_revision) ||
      !grepl("^[0-9a-fA-F]{40}$", source_revision)
  ) {
    source_revision <- paste(rep("0", 40L), collapse = "")
  }
  if (is.na(schema_version) || !nzchar(schema_version)) {
    schema_version <- "unknown"
  }
  list(
    package_id = package_id,
    repository = repository,
    source_revision = tolower(source_revision),
    package_schema_version = schema_version
  )
}

glc_explorer_declared_engine <- function(
  groups,
  variable_names = character(),
  variable_terms = character(),
  package = NULL,
  dataset_id = character(),
  file_group = character(),
  standardize = "lightlogr"
) {
  request <- glc_explorer_declared_request(
    variable_names,
    variable_terms,
    dataset_id,
    file_group,
    standardize
  )
  engine <- glc_declared_collection_engine(
    glc_explorer_declared_records(groups),
    request,
    glc_explorer_declared_provenance(package)
  )
  engine$request <- request
  engine
}

glc_explorer_filter_compatible_groups <- function(
  groups,
  variable_names = character(),
  variable_terms = character(),
  package = NULL,
  standardize = "lightlogr",
  declaration_groups = groups,
  dataset_id = character(),
  file_group = character()
) {
  variable_names <- glc_explorer_nonempty_values(variable_names)
  variable_terms <- glc_explorer_nonempty_values(variable_terms)
  active <- length(variable_names) > 0L || length(variable_terms) > 0L
  candidate_count <- nrow(groups)
  result <- list(
    active = active,
    candidate_count = candidate_count,
    matching_count = candidate_count,
    included_count = candidate_count,
    excluded_count = 0L,
    planner_request = glc_explorer_declared_request(
      variable_names,
      variable_terms,
      dataset_id,
      file_group,
      standardize
    ),
    groups = groups
  )
  if (!active || candidate_count == 0L) {
    return(result)
  }

  engine <- glc_explorer_declared_engine(
    declaration_groups,
    variable_names,
    variable_terms,
    package = package,
    dataset_id = dataset_id,
    file_group = file_group,
    standardize = standardize
  )
  candidate_ids <- glc_explorer_nonempty_values(groups$file_group_id)
  candidate_records <- engine$records[vapply(
    engine$records,
    function(record) record$file_group_id %in% candidate_ids,
    logical(1)
  )]
  matches <- vapply(
    candidate_records,
    function(record) {
      codes <- vapply(
        record$reasons,
        function(reason) reason$code,
        character(1)
      )
      !any(c("term_missing", "variable_missing") %in% codes)
    },
    logical(1)
  )
  result$matching_count <- sum(matches)
  included_ids <- vapply(
    engine$records,
    function(record) {
      if (
        identical(record$status, "included") &&
          identical(record$unit_id, engine$preferred_unit_id)
      ) {
        record$file_group_id
      } else {
        NA_character_
      }
    },
    character(1)
  )
  included_ids <- included_ids[!is.na(included_ids)]
  included <- groups[
    groups$file_group_id %in% included_ids,
    ,
    drop = FALSE
  ]

  result$included_count <- nrow(included)
  result$excluded_count <- candidate_count - nrow(included)
  result$groups <- included
  result
}

glc_explorer_selection_compatibility <- function(
  groups,
  variable_names,
  variable_terms = character(),
  package = NULL,
  standardize = "lightlogr",
  declaration_groups = groups,
  dataset_id = character(),
  file_group = character()
) {
  variable_names <- glc_explorer_nonempty_values(variable_names)
  variable_terms <- glc_explorer_nonempty_values(variable_terms)
  if (nrow(groups) == 0L) {
    return(list(ok = FALSE, issues = "No file groups are included."))
  }
  engine <- glc_explorer_declared_engine(
    declaration_groups,
    variable_names,
    variable_terms,
    package = package,
    dataset_id = dataset_id,
    file_group = file_group,
    standardize = standardize
  )
  records <- engine$records[vapply(
    engine$records,
    function(record) {
      codes <- vapply(
        record$reasons,
        function(reason) reason$code,
        character(1)
      )
      !any(c("scope_dataset", "scope_file_group") %in% codes)
    },
    logical(1)
  )]
  issues <- character()
  reason_codes <- lapply(records, function(record) {
    vapply(record$reasons, function(reason) reason$code, character(1))
  })
  unsupported <- vapply(
    seq_along(records),
    function(index) {
      if ("unsupported_format" %in% reason_codes[[index]]) {
        records[[index]]$format
      } else {
        NA_character_
      }
    },
    character(1)
  )
  unsupported <- glc_plan_sort_utf8(glc_explorer_nonempty_values(unsupported))
  if (length(unsupported) > 0L) {
    issues <- c(
      issues,
      paste0(
        "Included file groups use unsupported format(s): ",
        paste(unsupported, collapse = ", "),
        "."
      )
    )
  }
  if (
    any(vapply(
      reason_codes,
      function(codes) {
        "invalid_timezone" %in% codes
      },
      logical(1)
    ))
  ) {
    issues <- c(issues, "Included file groups contain invalid time zones.")
  }
  if (
    any(vapply(
      reason_codes,
      function(codes) {
        "incomplete_datetime" %in% codes
      },
      logical(1)
    ))
  ) {
    issues <- c(
      issues,
      "Included file groups contain incomplete datetime specifications."
    )
  }
  differences <- glc_declared_collection_record_differences(records)
  aggregate_codes <- c(
    "unsupported_format",
    "invalid_timezone",
    "incomplete_datetime",
    "included"
  )
  if ("columns" %in% differences) {
    aggregate_codes <- c(
      aggregate_codes,
      "term_missing",
      "variable_missing"
    )
  }
  for (record in records) {
    for (reason in record$reasons) {
      if (!reason$code %in% aggregate_codes) {
        issues <- c(
          issues,
          paste0("File group ", record$file_group_id, ": ", reason$message)
        )
      }
    }
  }
  difference_messages <- c(
    columns = "Included file groups use different source columns.",
    types_or_factor_levels = paste0(
      "Included file groups use different source variable types or ",
      "factor levels."
    ),
    timezone = "Included file groups use different time zones.",
    modalities = "Included file groups use different modalities.",
    role = "Included file groups use different file roles.",
    data_state = "Included file groups use different data states.",
    datetime = "Included file groups use different datetime specifications."
  )
  if (length(differences) > 0L) {
    issues <- c(issues, unname(difference_messages[differences]))
  }
  issues <- unique(issues)
  all_included <- all(vapply(
    records,
    function(record) {
      identical(record$status, "included")
    },
    logical(1)
  ))
  list(
    ok = all_included && length(engine$units) == 1L && length(issues) == 0L,
    issues = issues
  )
}

glc_explorer_package_selection_info <- function(package) {
  registry_row <- package$registry_row
  package_id <- glc_scalar_character(registry_row$id)
  if (is.na(package_id)) {
    package_id <- glc_scalar_character(package$descriptor$name)
  }
  if (is.na(package_id)) {
    package_id <- basename(package$repo)
  }
  latest_pass <- glc_scalar_character(registry_row$latest_pass_commit)
  generated_at <- glc_scalar_character(registry_row$registry_generated_at)
  list(
    package_id = package_id,
    repository = package$repo,
    commit = package$commit,
    latest_pass_commit = latest_pass,
    registry_generated_at = generated_at
  )
}

glc_explorer_script_source_issues <- function(package_info) {
  issues <- character()
  if (
    is.na(package_info$repository) ||
      !nzchar(package_info$repository)
  ) {
    issues <- c(issues, "The opened package has no GitHub repository.")
  }
  if (
    is.na(package_info$commit) ||
      !grepl("^[0-9a-fA-F]{40}$", package_info$commit)
  ) {
    issues <- c(
      issues,
      "The opened package has no exact 40-character commit SHA."
    )
  }
  if (
    is.na(package_info$latest_pass_commit) ||
      !identical(
        tolower(package_info$commit),
        tolower(package_info$latest_pass_commit)
      )
  ) {
    issues <- c(
      issues,
      "The opened revision is not the registry's latest passing commit."
    )
  }
  issues
}

glc_explorer_handoff_mode <- function(mode) {
  mode <- as.character(mode %||% "data")
  if (length(mode) != 1L || is.na(mode) || !mode %in% c("metadata", "data")) {
    glc_abort(
      "{.arg mode} must be one of {.val metadata} or {.val data}."
    )
  }
  mode
}

glc_explorer_row_limit <- function(value = Inf) {
  if (is.null(value) || length(value) == 0L) {
    return(Inf)
  }
  value <- suppressWarnings(as.numeric(value[[1L]]))
  if (
    is.na(value) || value <= 0 || (!is.infinite(value) && value != floor(value))
  ) {
    glc_abort(
      "{.arg n_max} must be a positive whole number or {.code Inf}."
    )
  }
  value
}

glc_explorer_metadata_resources <- function(package, resources = NULL) {
  inventory <- glc_resources(package)
  available <- unique(inventory$resource)
  if (
    is.null(resources) || length(glc_explorer_nonempty_values(resources)) == 0L
  ) {
    return(unique(inventory$resource[inventory$core %in% TRUE]))
  }
  resources <- glc_explorer_nonempty_values(resources)
  unknown <- setdiff(resources, available)
  if (length(unknown) > 0L) {
    glc_abort(
      "Unknown metadata resource{?s}: {.val {unknown}}."
    )
  }
  unique(resources)
}

glc_explorer_selection_hash <- function(
  package_info,
  datasets,
  groups,
  mode = "data",
  metadata_resources = character()
) {
  value <- list(
    repository = package_info$repository,
    commit = package_info$commit,
    mode = glc_explorer_handoff_mode(mode),
    dataset_ids = sort(unique(datasets)),
    file_group_ids = sort(unique(groups)),
    metadata_resources = sort(unique(metadata_resources))
  )
  substr(digest::digest(value, algo = "xxhash64", serialize = TRUE), 1L, 10L)
}

glc_explorer_safe_path_component <- function(value) {
  value <- tolower(as.character(value))
  value <- gsub("[^a-z0-9._-]+", "-", value)
  value <- gsub("(^-+|-+$)", "", value)
  if (!nzchar(value)) "glc-package" else value
}

glc_explorer_build_selection_plan <- function(
  package,
  selection,
  facets,
  mode = c("data", "metadata"),
  metadata_resources = NULL,
  participant_ids = character(),
  device_ids = character(),
  dataset_ids = character(),
  group_filters = NULL,
  file_group_ids = character(),
  variables = character(),
  terms = character(),
  n_max = Inf,
  standardize = c("lightlogr", "none"),
  resolved_scope = NULL,
  resolved_group_discovery = NULL
) {
  mode <- glc_explorer_handoff_mode(mode[[1L]])
  standardize <- match.arg(standardize)
  n_max <- glc_explorer_row_limit(n_max)
  package_info <- glc_explorer_package_selection_info(package)
  source_issues <- glc_explorer_script_source_issues(package_info)

  if (identical(mode, "metadata")) {
    metadata_resources <- glc_explorer_metadata_resources(
      package,
      metadata_resources
    )
    selection_hash <- glc_explorer_selection_hash(
      package_info,
      character(),
      character(),
      mode = mode,
      metadata_resources = metadata_resources
    )
    directory_name <- paste(
      glc_explorer_safe_path_component(package_info$package_id),
      substr(package_info$commit, 1L, 12L),
      selection_hash,
      sep = "-"
    )
    return(list(
      mode = mode,
      package_id = package_info$package_id,
      repository = package_info$repository,
      commit = package_info$commit,
      registry_generated_at = package_info$registry_generated_at,
      facets = facets,
      metadata_resources = metadata_resources,
      requested = list(
        participant_ids = character(),
        device_ids = character(),
        dataset_ids = character(),
        group_filters = NULL,
        file_group_ids = character(),
        variables = character(),
        terms = character()
      ),
      planner_restrictions = list(
        dataset_id = character(),
        file_group = character(),
        dataset_id_explicit = FALSE,
        file_group_basis = "omitted"
      ),
      read_restrictions = list(
        dataset_id = character(),
        file_group = character()
      ),
      participants = character(),
      devices = character(),
      datasets = character(),
      file_groups = character(),
      variables = character(),
      terms = character(),
      variable_filter = NULL,
      term_filter = NULL,
      name_filter_active = FALSE,
      term_filter_active = FALSE,
      variable_filter_active = FALSE,
      group_discovery = list(
        active = FALSE,
        candidate_count = 0L,
        included_count = 0L,
        excluded_count = 0L,
        dataset_count = 0L,
        filters = NULL
      ),
      group_filter = list(
        active = FALSE,
        candidate_count = 0L,
        matching_count = 0L,
        included_count = 0L,
        excluded_count = 0L
      ),
      files = tibble::tibble(),
      estimated_bytes = 0,
      unknown_file_sizes = 0L,
      preview_files = character(),
      n_max = Inf,
      standardization = standardize,
      compatibility = list(ok = TRUE, issues = character()),
      issues = character(),
      script_issues = source_issues,
      preview_ready = FALSE,
      script_ready = length(source_issues) == 0L,
      selection_hash = selection_hash,
      data_directory = file.path("metadata", directory_name)
    ))
  }

  scope <- resolved_scope
  if (is.null(scope)) {
    scope <- glc_explorer_selection_scope(
      selection,
      facets,
      participant_ids = participant_ids,
      device_ids = device_ids
    )
  }
  requested_datasets <- glc_explorer_nonempty_values(dataset_ids)
  selected_datasets <- selection$datasets$dataset_id[
    selection$datasets$dataset_id %in%
      intersect(scope$dataset_ids, requested_datasets)
  ]
  available_groups <- selection$groups[
    selection$groups$dataset_id %in% selected_datasets,
    ,
    drop = FALSE
  ]
  if (scope$device_restricted) {
    available_groups <- available_groups[
      !is.na(available_groups$device_id) &
        available_groups$device_id %in% scope$device_ids,
      ,
      drop = FALSE
    ]
  }
  group_discovery <- resolved_group_discovery
  valid_discovery <- is.list(group_discovery) &&
    inherits(group_discovery$groups, "data.frame")
  if (!valid_discovery) {
    group_discovery <- glc_explorer_selection_group_filter_result(
      available_groups,
      group_filters
    )
  }
  available_groups <- group_discovery$groups
  group_discovery$groups <- NULL
  requested_groups <- glc_explorer_nonempty_values(file_group_ids)
  candidate_groups <- available_groups
  if (length(requested_groups) > 0L) {
    candidate_groups <- candidate_groups[
      candidate_groups$file_group_id %in% requested_groups,
      ,
      drop = FALSE
    ]
  }
  planner_restrictions <- glc_explorer_planner_restrictions(
    requested_datasets,
    candidate_groups,
    participant_restricted = scope$participant_restricted,
    device_restricted = scope$device_restricted,
    group_filter_active = group_discovery$active,
    requested_file_group_ids = requested_groups
  )
  requested_variables <- glc_explorer_nonempty_values(variables)
  requested_terms <- glc_explorer_nonempty_values(terms)
  group_filter <- glc_explorer_filter_compatible_groups(
    candidate_groups,
    requested_variables,
    requested_terms,
    package = package,
    standardize = standardize,
    declaration_groups = selection$groups,
    dataset_id = planner_restrictions$dataset_id,
    file_group = planner_restrictions$file_group
  )
  groups <- group_filter$groups
  group_filter$groups <- NULL
  resolved_dataset_ids <- unique(groups$dataset_id)
  group_ids <- unique(groups$file_group_id)
  read_restrictions <- list(
    dataset_id = glc_plan_sort_utf8(resolved_dataset_ids),
    file_group = glc_plan_sort_utf8(group_ids)
  )
  available_variables <- selection$variables[
    selection$variables$file_group_id %in% group_ids,
    ,
    drop = FALSE
  ]
  selected_variable_rows <- glc_explorer_selected_variable_rows(
    available_variables,
    requested_variables,
    requested_terms
  )
  selected_variables <- unique(selected_variable_rows$name)
  name_filter_active <- length(requested_variables) > 0L
  term_filter_active <- length(requested_terms) > 0L
  variable_filter_active <- name_filter_active || term_filter_active
  variable_filter <- if (name_filter_active) {
    selected_variables
  } else {
    NULL
  }
  term_filter <- if (term_filter_active && !name_filter_active) {
    requested_terms
  } else {
    NULL
  }
  files <- selection$files[
    selection$files$file_group_id %in% group_ids,
    ,
    drop = FALSE
  ]
  preview_files <- files$declared_path[!duplicated(files$file_group_id)]
  preview_files <- glc_explorer_nonempty_values(preview_files)

  issues <- character()
  if (length(selected_datasets) == 0L) {
    issues <- c(issues, "Select at least one eligible dataset.")
  }
  if (length(selected_datasets) > 0L && nrow(candidate_groups) == 0L) {
    issues <- c(
      issues,
      if (
        isTRUE(group_discovery$active) &&
          identical(group_discovery$included_count, 0L)
      ) {
        "No file groups match the current file-group field filters."
      } else if (length(requested_groups) > 0L) {
        "None of the chosen file groups is available for the current filters."
      } else {
        "No file groups are available for the selected datasets."
      }
    )
  } else if (
    length(selected_datasets) > 0L &&
      isTRUE(group_filter$active) &&
      nrow(groups) == 0L
  ) {
    issues <- c(
      issues,
      paste0(
        "No eligible file groups provide a compatible complete match for ",
        "the chosen source variables or semantic terms."
      )
    )
  }
  compatibility <- if (nrow(groups) > 0L) {
    glc_explorer_selection_compatibility(
      groups,
      requested_variables,
      requested_terms,
      package = package,
      standardize = standardize,
      declaration_groups = selection$groups,
      dataset_id = read_restrictions$dataset_id,
      file_group = read_restrictions$file_group
    )
  } else {
    list(ok = FALSE, issues = character())
  }
  issues <- unique(c(issues, compatibility$issues))
  script_issues <- unique(c(
    issues,
    source_issues
  ))
  resolved_datasets <- selection$datasets[
    selection$datasets$dataset_id %in% resolved_dataset_ids,
    ,
    drop = FALSE
  ]
  resolved_participants <- glc_explorer_nonempty_values(
    resolved_datasets$participant_id
  )
  resolved_devices <- glc_explorer_nonempty_values(groups$device_id)
  known_bytes <- !is.na(files$expected_bytes)
  estimated_bytes <- sum(files$expected_bytes[known_bytes], na.rm = TRUE)
  selection_hash <- glc_explorer_selection_hash(
    package_info,
    resolved_dataset_ids,
    group_ids,
    mode = mode
  )
  directory_name <- paste(
    glc_explorer_safe_path_component(package_info$package_id),
    substr(package_info$commit, 1L, 12L),
    selection_hash,
    sep = "-"
  )

  list(
    mode = mode,
    package_id = package_info$package_id,
    repository = package_info$repository,
    commit = package_info$commit,
    registry_generated_at = package_info$registry_generated_at,
    facets = facets,
    metadata_resources = character(),
    requested = list(
      participant_ids = glc_explorer_nonempty_values(participant_ids),
      device_ids = glc_explorer_nonempty_values(device_ids),
      dataset_ids = requested_datasets,
      group_filters = group_discovery$filters,
      file_group_ids = requested_groups,
      variables = requested_variables,
      terms = requested_terms
    ),
    planner_restrictions = planner_restrictions,
    read_restrictions = read_restrictions,
    participants = resolved_participants,
    devices = resolved_devices,
    datasets = resolved_dataset_ids,
    file_groups = group_ids,
    variables = selected_variables,
    terms = requested_terms,
    variable_filter = variable_filter,
    term_filter = term_filter,
    name_filter_active = name_filter_active,
    term_filter_active = term_filter_active,
    variable_filter_active = variable_filter_active,
    group_discovery = group_discovery,
    group_filter = group_filter,
    files = files,
    estimated_bytes = estimated_bytes,
    unknown_file_sizes = sum(!known_bytes),
    preview_files = preview_files,
    n_max = n_max,
    standardization = standardize,
    compatibility = compatibility,
    issues = issues,
    script_issues = script_issues,
    preview_ready = length(issues) == 0L,
    script_ready = length(script_issues) == 0L,
    selection_hash = selection_hash,
    data_directory = file.path("data", directory_name)
  )
}

glc_explorer_r_literal <- function(value) {
  paste(
    deparse(
      value,
      width.cutoff = 100L
    ),
    collapse = "\n"
  )
}

glc_explorer_cleanup_lines <- function(temporary_objects, section) {
  object_lines <- strsplit(
    glc_explorer_r_literal(unique(temporary_objects)),
    "\n",
    fixed = TRUE
  )[[1L]]
  object_lines <- paste0("    ", object_lines)
  object_lines[[length(object_lines)]] <- paste0(
    object_lines[[length(object_lines)]],
    ","
  )

  c(
    sprintf(
      "# ---- %d. Clean up temporary handoff objects ----",
      as.integer(section)
    ),
    "# Keep only the imported result and local package handle.",
    "rm(",
    "  list = intersect(",
    object_lines,
    "    ls(envir = environment(), all.names = TRUE)",
    "  ),",
    "  envir = environment()",
    ")"
  )
}

glc_explorer_metadata_script <- function(plan) {
  registry_timestamp <- if (is.na(plan$registry_generated_at)) {
    "not recorded"
  } else {
    plan$registry_generated_at
  }
  lines <- c(
    "# Generated by the GLC data explorer.",
    paste0("# Registry timestamp: ", registry_timestamp),
    paste0("# Exact latest-passing SHA: ", plan$commit),
    "# This script downloads package metadata only; it imports no data files.",
    "# glcdp functions are called explicitly and no packages are attached.",
    "",
    "# ---- 1. Reproducible package settings ----",
    paste0("repository <- ", glc_explorer_r_literal(plan$repository)),
    paste0("commit_sha <- ", glc_explorer_r_literal(plan$commit)),
    paste0(
      "metadata_resources <- ",
      glc_explorer_r_literal(plan$metadata_resources)
    ),
    paste0(
      "metadata_dir <- ",
      glc_explorer_r_literal(plan$data_directory)
    ),
    "",
    "# ---- 2. Download the selected metadata when needed ----",
    "# Reuse an existing manifest-backed directory to avoid downloading twice.",
    "manifest_path <- file.path(metadata_dir, \"glcdp-manifest.json\")",
    "if (!file.exists(manifest_path)) {",
    paste0(
      "  remote_package <- glcdp::glc_open(",
      "repository, ref = commit_sha, quiet = TRUE)"
    ),
    "  glcdp::glc_download(",
    "    remote_package,",
    "    dest_dir = metadata_dir,",
    "    include = \"metadata\",",
    "    resources = metadata_resources,",
    "    overwrite = FALSE",
    "  )",
    "}",
    "",
    "# ---- 3. Open the local package and load its metadata ----",
    "local_package <- glcdp::glc_open(metadata_dir, quiet = TRUE)",
    paste0(
      "glc_metadata <- glcdp::glc_metadata(",
      "local_package, resources = metadata_resources)"
    ),
    "",
    glc_explorer_cleanup_lines(
      c(
        "repository",
        "commit_sha",
        "metadata_resources",
        "metadata_dir",
        "manifest_path",
        "remote_package"
      ),
      section = 4L
    ),
    "",
    "# local_package is the package handle; glc_metadata is a named metadata list."
  )
  paste(lines, collapse = "\n")
}

glc_explorer_selection_script <- function(plan) {
  if (!isTRUE(plan$script_ready)) {
    glc_abort(
      paste(
        "Cannot generate the R handoff script:",
        paste(plan$script_issues, collapse = " ")
      ),
      class = "glcdp_explorer_invalid_selection"
    )
  }
  if (identical(plan$mode %||% "data", "metadata")) {
    return(glc_explorer_metadata_script(plan))
  }
  registry_timestamp <- if (is.na(plan$registry_generated_at)) {
    "not recorded"
  } else {
    plan$registry_generated_at
  }
  variable_comment <- if (isTRUE(plan$variable_filter_active)) {
    "# Complete included files are downloaded before variable filtering."
  } else {
    "# No variable filter is applied; all declared source variables are imported."
  }
  lines <- c(
    "# Generated by the GLC data explorer.",
    paste0("# Registry timestamp: ", registry_timestamp),
    paste0("# Exact latest-passing SHA: ", plan$commit),
    variable_comment,
    "# This script calls glcdp functions explicitly and does not attach packages.",
    "",
    "# ---- 1. Reproducible selection settings ----",
    "# These values describe exactly what was selected in the app.",
    "# The commit SHA pins the download to the verified package revision.",
    paste0("repository <- ", glc_explorer_r_literal(plan$repository)),
    paste0("commit_sha <- ", glc_explorer_r_literal(plan$commit)),
    paste0(
      "selection_facets <- ",
      glc_explorer_r_literal(plan$facets)
    ),
    paste0(
      "participant_ids <- ",
      glc_explorer_r_literal(plan$participants)
    ),
    paste0("device_ids <- ", glc_explorer_r_literal(plan$devices)),
    paste0("dataset_ids <- ", glc_explorer_r_literal(plan$datasets)),
    paste0("file_groups <- ", glc_explorer_r_literal(plan$file_groups)),
    paste0(
      "source_variables <- ",
      glc_explorer_r_literal(plan$variable_filter)
    ),
    paste0(
      "source_terms <- ",
      glc_explorer_r_literal(plan$term_filter)
    ),
    paste0("row_limit <- ", glc_explorer_r_literal(plan$n_max %||% Inf)),
    paste0("data_dir <- ", glc_explorer_r_literal(plan$data_directory)),
    "",
    "# ---- 2. Download the selected files when needed ----",
    "# Reuse an existing manifest-backed directory to avoid downloading twice.",
    "manifest_path <- file.path(data_dir, \"glcdp-manifest.json\")",
    "if (!file.exists(manifest_path)) {",
    paste0(
      "  remote_package <- glcdp::glc_open(",
      "repository, ref = commit_sha, quiet = TRUE)"
    ),
    "  glcdp::glc_download(",
    "    remote_package,",
    "    dest_dir = data_dir,",
    "    include = \"data\",",
    "    dataset_id = dataset_ids,",
    "    file_group = file_groups,",
    "    overwrite = FALSE",
    "  )",
    "}",
    "",
    "# ---- 3. Open the local data package ----",
    "# From this point onward, reading uses the reproducible local copy.",
    "local_package <- glcdp::glc_open(data_dir, quiet = TRUE)",
    "",
    "# ---- 4. Define the requested data read ----",
    "# glc_read() validates the requested datasets, groups, and variables.",
    "glc_selection <- glcdp::glc_read(",
    "  local_package,",
    "  dataset_id = dataset_ids,",
    "  file_group = file_groups,",
    "  variables = source_variables,",
    "  terms = source_terms,",
    "  n_max = row_limit",
    ")",
    "",
    "# ---- 5. Import and combine the data ----",
    "# glc_collect() materializes the files and applies the chosen column mode.",
    paste0(
      "glc_data <- glcdp::glc_collect(",
      "glc_selection, standardize = ",
      glc_explorer_r_literal(plan$standardization),
      ")"
    ),
    "",
    glc_explorer_cleanup_lines(
      c(
        "repository",
        "commit_sha",
        "selection_facets",
        "participant_ids",
        "device_ids",
        "dataset_ids",
        "file_groups",
        "source_variables",
        "source_terms",
        "row_limit",
        "data_dir",
        "manifest_path",
        "remote_package",
        "glc_selection"
      ),
      section = 6L
    ),
    "",
    "# glc_data is the final imported table and is ready for analysis."
  )
  paste(lines, collapse = "\n")
}

glc_explorer_preview_row_limit <- function(value, default = 10L) {
  if (length(value) == 0L || is.null(value) || is.na(value[[1L]])) {
    return(as.integer(default))
  }
  value <- suppressWarnings(as.integer(value[[1L]]))
  if (is.na(value)) {
    return(as.integer(default))
  }
  max(1L, min(1000L, value))
}

glc_explorer_preview_file_limit <- function(
  value,
  available,
  default = 2L
) {
  if (length(available) == 0L || is.null(available)) {
    return(0L)
  }
  available <- suppressWarnings(as.integer(available[[1L]] %||% 0L))
  if (is.na(available) || available < 1L) {
    return(0L)
  }
  if (length(value) == 0L || is.null(value) || is.na(value[[1L]])) {
    value <- default
  }
  value <- suppressWarnings(as.integer(value[[1L]]))
  if (is.na(value)) {
    value <- as.integer(default)
  }
  max(1L, min(available, value))
}

glc_explorer_preview_files <- function(plan, file_limit = 2L) {
  available <- glc_explorer_nonempty_values(plan$preview_files)
  limit <- glc_explorer_preview_file_limit(
    file_limit,
    length(available)
  )
  utils::head(available, limit)
}

glc_explorer_preview_transfer <- function(plan, file_limit = 2L) {
  files <- glc_explorer_preview_files(plan, file_limit)
  inventory <- plan$files
  if (
    !inherits(inventory, "data.frame") ||
      !all(c("declared_path", "expected_bytes") %in% names(inventory))
  ) {
    return(list(
      files = files,
      available = length(glc_explorer_nonempty_values(plan$preview_files)),
      estimated_bytes = 0,
      unknown_file_sizes = length(files)
    ))
  }
  file_rows <- inventory[
    match(files, inventory$declared_path, nomatch = 0L),
    ,
    drop = FALSE
  ]
  known_bytes <- !is.na(file_rows$expected_bytes)
  list(
    files = files,
    available = length(glc_explorer_nonempty_values(plan$preview_files)),
    estimated_bytes = sum(
      file_rows$expected_bytes[known_bytes],
      na.rm = TRUE
    ),
    unknown_file_sizes = sum(!known_bytes)
  )
}

glc_explorer_preview_selection <- function(
  package,
  plan,
  n_max = 10L,
  file_limit = 2L
) {
  if (!isTRUE(plan$preview_ready)) {
    glc_abort(
      paste(
        "Cannot build the preview:",
        paste(plan$issues, collapse = " ")
      ),
      class = "glcdp_explorer_invalid_selection"
    )
  }
  preview_limit <- glc_explorer_preview_row_limit(n_max)
  final_limit <- plan$n_max %||% Inf
  if (is.finite(final_limit)) {
    preview_limit <- min(preview_limit, final_limit)
  }
  preview_files <- glc_explorer_preview_files(plan, file_limit)
  collection <- glc_read(
    package,
    dataset_id = plan$datasets,
    file_group = plan$file_groups,
    files = preview_files,
    variables = plan$variable_filter,
    terms = plan$term_filter,
    n_max = preview_limit,
    progress = FALSE
  )
  result <- glc_collect(collection, standardize = plan$standardization)
  tibble::as_tibble(result)
}

glc_explorer_format_preview <- function(data) {
  if (is.null(data)) return(NULL)
  result <- data
  datetime_columns <- names(result)[vapply(
    result,
    inherits,
    logical(1),
    what = "POSIXct"
  )]
  for (name in datetime_columns) {
    timezone <- attr(result[[name]], "tzone")
    if (is.null(timezone) || length(timezone) == 0L || is.na(timezone[[1L]])) {
      timezone <- "UTC"
    } else {
      timezone <- timezone[[1L]]
    }
    result[[name]] <- format(
      result[[name]],
      format = "%Y-%m-%d %H:%M:%S %Z",
      tz = timezone
    )
  }
  result
}

glc_explorer_download_filename <- function(plan) {
  suffix <- if (identical(plan$mode %||% "data", "metadata")) {
    "-metadata.R"
  } else {
    "-selection.R"
  }
  paste0(
    glc_explorer_safe_path_component(plan$package_id),
    suffix
  )
}

glc_explorer_download_complete_modal <- function(filename, mode = "data") {
  mode <- glc_explorer_handoff_mode(mode)
  description <- if (identical(mode, "metadata")) {
    paste0(
      " in R to download the selected package metadata and load both the ",
      "package handle and metadata list."
    )
  } else {
    paste0(
      " in R to download, import, and collect the data exactly as ",
      "specified in your selection."
    )
  }
  shiny::modalDialog(
    title = shiny::tagList(
      shiny::icon("circle-check"),
      "R script downloaded"
    ),
    shiny::tags$p(
      "Use ",
      shiny::tags$code(filename),
      description
    ),
    shiny::tags$p("You can now:"),
    shiny::tags$ul(
      shiny::tags$li("adjust the current selection and export another script;"),
      shiny::tags$li("open a different data package from the Registry; or"),
      shiny::tags$li("close this browser tab or app when you are finished.")
    ),
    footer = shiny::modalButton("Continue exploring"),
    easyClose = TRUE
  )
}

glc_explorer_choice_values <- function(value) {
  value <- sort(glc_explorer_nonempty_values(value))
  stats::setNames(value, value)
}

glc_explorer_preserve_selected_choices <- function(
  choices,
  selected,
  universe = choices
) {
  normalize <- function(value) {
    labels <- names(value)
    value <- as.character(value)
    if (is.null(labels) || length(labels) != length(value)) {
      labels <- value
    } else {
      missing <- is.na(labels) | !nzchar(labels)
      labels[missing] <- value[missing]
    }
    stats::setNames(value, labels)
  }

  choices <- normalize(choices)
  universe <- normalize(universe)
  selected <- intersect(
    glc_explorer_nonempty_values(selected),
    unname(universe)
  )
  missing <- setdiff(selected, unname(choices))
  if (length(missing) > 0L) {
    choices <- c(
      choices,
      universe[match(missing, unname(universe))]
    )
  }
  choices[!duplicated(unname(choices))]
}

glc_explorer_variable_term_choices <- function(variables) {
  if (
    !inherits(variables, "data.frame") ||
      nrow(variables) == 0L ||
      !"term" %in% names(variables)
  ) {
    return(character())
  }
  terms <- sort(glc_explorer_nonempty_values(variables$term))
  term_names <- if ("term_name" %in% names(variables)) {
    vapply(
      terms,
      function(term) {
        names <- glc_explorer_nonempty_values(
          variables$term_name[variables$term == term]
        )
        if (length(names) == 0L) "" else names[[1L]]
      },
      character(1)
    )
  } else {
    rep("", length(terms))
  }
  labels <- ifelse(
    nzchar(term_names),
    paste(terms, term_names, sep = " \u2014 "),
    terms
  )
  stats::setNames(terms, labels)
}

glc_explorer_default_variables <- function(variables) {
  primary <- unique(variables$name[variables$primary %in% TRUE])
  if (length(primary) > 0L) primary else unique(variables$name)
}

glc_explorer_selection_group_filter_spec <- function(
  device_ids = character(),
  device_locations = character(),
  location_types = character(),
  modalities = character(),
  roles = character(),
  data_states = character(),
  variable_names = character(),
  variable_terms = character()
) {
  filters <- list(
    device_ids = glc_explorer_nonempty_values(device_ids),
    device_locations = glc_explorer_nonempty_values(device_locations),
    location_types = glc_explorer_nonempty_values(location_types),
    modalities = glc_explorer_nonempty_values(modalities),
    roles = glc_explorer_nonempty_values(roles),
    data_states = glc_explorer_nonempty_values(data_states),
    variable_names = glc_explorer_nonempty_values(variable_names),
    variable_terms = glc_explorer_nonempty_values(variable_terms)
  )
  filters$active <- any(lengths(filters) > 0L)
  filters
}

glc_explorer_normalize_selection_group_filters <- function(filters = NULL) {
  filters <- filters %||% list()
  glc_explorer_selection_group_filter_spec(
    device_ids = filters$device_ids,
    device_locations = filters$device_locations,
    location_types = filters$location_types,
    modalities = filters$modalities,
    roles = filters$roles,
    data_states = filters$data_states,
    variable_names = filters$variable_names,
    variable_terms = filters$variable_terms
  )
}

glc_explorer_selection_group_column <- function(groups, name) {
  if (!name %in% names(groups)) {
    return(rep(NA_character_, nrow(groups)))
  }
  as.character(groups[[name]])
}

glc_explorer_selection_group_list_column <- function(groups, name) {
  if (!name %in% names(groups)) {
    return(rep(list(character()), nrow(groups)))
  }
  groups[[name]]
}

glc_explorer_selection_group_filter_result <- function(
  groups,
  filters = NULL
) {
  filters <- glc_explorer_normalize_selection_group_filters(filters)
  candidate_count <- nrow(groups)
  keep <- rep(TRUE, candidate_count)
  scalar_fields <- list(
    device_ids = "device_id",
    device_locations = "device_location",
    location_types = "device_location_type",
    roles = "role",
    data_states = "data_state"
  )
  for (filter_name in names(scalar_fields)) {
    selected <- filters[[filter_name]]
    if (length(selected) > 0L) {
      values <- glc_explorer_selection_group_column(
        groups,
        scalar_fields[[filter_name]]
      )
      keep <- keep & !is.na(values) & nzchar(values) & values %in% selected
    }
  }
  if (length(filters$modalities) > 0L) {
    values <- glc_explorer_selection_group_list_column(groups, "modalities")
    keep <- keep &
      vapply(
        values,
        function(value) {
          any(glc_explorer_nonempty_values(value) %in% filters$modalities)
        },
        logical(1)
      )
  }
  if (
    length(filters$variable_names) > 0L ||
      length(filters$variable_terms) > 0L
  ) {
    cached_names <- if ("variable_names" %in% names(groups)) {
      groups$variable_names
    } else {
      NULL
    }
    cached_terms <- if ("variable_terms" %in% names(groups)) {
      groups$variable_terms
    } else {
      NULL
    }
    variables <- if (is.null(cached_names) || is.null(cached_terms)) {
      glc_explorer_selection_group_list_column(groups, "variables")
    } else {
      NULL
    }
    keep <- keep &
      vapply(
        seq_len(candidate_count),
        function(index) {
          if (!is.null(variables)) {
            value <- variables[[index]]
            if (!inherits(value, "data.frame")) {
              return(FALSE)
            }
            names <- if ("name" %in% names(value)) {
              glc_explorer_nonempty_values(value$name)
            } else {
              character()
            }
            terms <- if ("term" %in% names(value)) {
              glc_explorer_nonempty_values(value$term)
            } else {
              character()
            }
          } else {
            names <- cached_names[[index]]
            terms <- cached_terms[[index]]
          }
          all(filters$variable_names %in% names) &&
            all(filters$variable_terms %in% terms)
        },
        logical(1)
      )
  }
  filtered <- groups[keep, , drop = FALSE]
  list(
    active = isTRUE(filters$active),
    candidate_count = candidate_count,
    included_count = nrow(filtered),
    excluded_count = candidate_count - nrow(filtered),
    dataset_count = length(glc_explorer_nonempty_values(
      filtered$dataset_id
    )),
    filters = filters,
    groups = filtered
  )
}

glc_explorer_selection_group_field_choices <- function(groups) {
  nested_values <- function(column) {
    values <- glc_explorer_selection_group_list_column(groups, "variables")
    unlist(
      lapply(values, function(value) {
        if (!inherits(value, "data.frame") || !column %in% names(value)) {
          return(character())
        }
        value[[column]]
      }),
      use.names = FALSE
    )
  }
  list(
    device_ids = glc_explorer_choice_values(
      glc_explorer_selection_group_column(groups, "device_id")
    ),
    device_locations = glc_explorer_choice_values(
      glc_explorer_selection_group_column(groups, "device_location")
    ),
    location_types = glc_explorer_choice_values(
      glc_explorer_selection_group_column(groups, "device_location_type")
    ),
    modalities = glc_explorer_choice_values(unlist(
      glc_explorer_selection_group_list_column(groups, "modalities"),
      use.names = FALSE
    )),
    roles = glc_explorer_choice_values(
      glc_explorer_selection_group_column(groups, "role")
    ),
    data_states = glc_explorer_choice_values(
      glc_explorer_selection_group_column(groups, "data_state")
    ),
    variable_names = glc_explorer_choice_values(
      if ("variable_names" %in% names(groups)) {
        unlist(groups$variable_names, use.names = FALSE)
      } else {
        nested_values("name")
      }
    ),
    variable_terms = glc_explorer_variable_term_choices(
      tibble::tibble(
        term = nested_values("term"),
        term_name = nested_values("term_name")
      )
    )
  )
}

glc_explorer_file_group_choices <- function(groups) {
  if (nrow(groups) == 0L) {
    return(character())
  }
  details <- vapply(
    seq_len(nrow(groups)),
    function(index) {
      glc_explorer_nonempty_values(c(
        groups$device_id[[index]],
        groups$role[[index]],
        groups$data_state[[index]],
        groups$format[[index]]
      )) |>
        paste(collapse = " \u00b7 ")
    },
    character(1)
  )
  labels <- ifelse(
    nzchar(details),
    paste(groups$file_group_id, details, sep = " \u2014 "),
    groups$file_group_id
  )
  stats::setNames(groups$file_group_id, labels)
}

glc_explorer_resolved_group_rows <- function(
  selection,
  scope,
  dataset_ids,
  file_group_ids = character(),
  group_filters = NULL
) {
  selected_datasets <- intersect(
    scope$dataset_ids,
    glc_explorer_nonempty_values(dataset_ids)
  )
  groups <- selection$groups[
    selection$groups$dataset_id %in% selected_datasets,
    ,
    drop = FALSE
  ]
  if (scope$device_restricted) {
    groups <- groups[
      !is.na(groups$device_id) & groups$device_id %in% scope$device_ids,
      ,
      drop = FALSE
    ]
  }
  groups <- glc_explorer_selection_group_filter_result(
    groups,
    group_filters
  )$groups
  requested_groups <- glc_explorer_nonempty_values(file_group_ids)
  if (length(requested_groups) > 0L) {
    groups <- groups[
      groups$file_group_id %in% requested_groups,
      ,
      drop = FALSE
    ]
  }
  groups
}

glc_explorer_available_variables <- function(
  selection,
  scope,
  dataset_ids,
  file_group_ids = character(),
  group_filters = NULL
) {
  groups <- glc_explorer_resolved_group_rows(
    selection,
    scope,
    dataset_ids,
    file_group_ids,
    group_filters
  )
  selection$variables[
    selection$variables$file_group_id %in% groups$file_group_id,
    ,
    drop = FALSE
  ]
}

glc_explorer_format_bytes <- function(bytes, unknown = 0L) {
  bytes <- as.numeric(bytes)
  units <- c("B", "KB", "MB", "GB", "TB")
  unit <- 1L
  value <- bytes
  while (is.finite(value) && value >= 1000 && unit < length(units)) {
    value <- value / 1000
    unit <- unit + 1L
  }
  known <- if (is.finite(value)) {
    paste0(formatC(value, digits = 3L, format = "fg"), " ", units[[unit]])
  } else {
    "unknown"
  }
  if (unknown > 0L) {
    paste0(known, " plus ", unknown, " file(s) of unknown size")
  } else {
    known
  }
}

glc_explorer_display_selection_values <- function(values, limit = 8L) {
  values <- glc_explorer_nonempty_values(values)
  limit <- suppressWarnings(as.integer(limit[[1L]] %||% 8L))
  if (is.na(limit) || limit < 1L) {
    limit <- 8L
  }
  if (length(values) <= limit) {
    return(glc_explorer_display_values(values))
  }
  paste0(
    paste(utils::head(values, limit), collapse = ", "),
    " \u2026 (+",
    length(values) - limit,
    " more)"
  )
}

glc_explorer_selection_summary_table <- function(plan) {
  if (identical(plan$mode %||% "data", "metadata")) {
    return(data.frame(
      Selection = c(
        "Handoff",
        "Repository",
        "Exact revision",
        "Metadata resources",
        "Relative metadata directory"
      ),
      Value = c(
        "Package and metadata only",
        plan$repository,
        plan$commit,
        glc_explorer_display_selection_values(plan$metadata_resources),
        plan$data_directory
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  variable_selection <- if (isTRUE(plan$variable_filter_active)) {
    glc_explorer_display_selection_values(plan$variables)
  } else {
    paste0("All available (", length(plan$variables), ")")
  }
  term_selection <- if (isTRUE(plan$term_filter_active)) {
    glc_explorer_display_selection_values(plan$terms)
  } else {
    "Not filtered"
  }
  data.frame(
    Selection = c(
      "Participants",
      "Devices",
      "Datasets",
      "File groups",
      "Source variables",
      "Semantic terms",
      "Files",
      "Preview files",
      "Estimated transfer",
      "Maximum rows per file",
      "Collection mode",
      "Relative data directory"
    ),
    Value = c(
      glc_explorer_display_selection_values(plan$participants),
      glc_explorer_display_selection_values(plan$devices),
      glc_explorer_display_selection_values(plan$datasets),
      glc_explorer_display_selection_values(plan$file_groups),
      variable_selection,
      term_selection,
      nrow(plan$files),
      length(plan$preview_files),
      glc_explorer_format_bytes(
        plan$estimated_bytes,
        plan$unknown_file_sizes
      ),
      if (is.finite(plan$n_max %||% Inf)) {
        format(plan$n_max, big.mark = ",", scientific = FALSE)
      } else {
        "All rows"
      },
      if (identical(plan$standardization, "lightlogr")) {
        "LightLogR-compatible"
      } else {
        "Source columns"
      },
      plan$data_directory
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

glc_explorer_selection_group_table <- function(selection, plan) {
  groups <- selection$groups[
    selection$groups$file_group_id %in% plan$file_groups,
    ,
    drop = FALSE
  ]
  location <- glc_explorer_selection_group_column(groups, "device_location")
  data.frame(
    Dataset = groups$dataset_id,
    `File group` = groups$file_group_id,
    Device = ifelse(
      is.na(groups$device_id) | !nzchar(groups$device_id),
      "\u2014",
      groups$device_id
    ),
    `Wearing position` = ifelse(
      is.na(location) | !nzchar(location),
      "\u2014",
      location
    ),
    Format = groups$format,
    `Time zone` = groups$timezone,
    Modalities = vapply(
      groups$modalities,
      glc_explorer_display_values,
      character(1)
    ),
    Role = groups$role,
    State = groups$data_state,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

glc_explorer_selection_issues_tag <- function(plan) {
  if (identical(plan$mode %||% "data", "metadata")) {
    if (isTRUE(plan$script_ready)) {
      return(shiny::tags$div(
        class = "alert alert-success py-2",
        role = "status",
        shiny::icon("circle-check"),
        sprintf(
          " Package metadata is ready to export for %d resource(s).",
          length(plan$metadata_resources)
        )
      ))
    }
    return(shiny::tags$div(
      class = "alert alert-warning py-2",
      role = "status",
      shiny::icon("triangle-exclamation"),
      shiny::tags$strong(" Metadata handoff needs attention"),
      shiny::tags$ul(
        class = "mb-0 mt-1",
        lapply(plan$script_issues, shiny::tags$li)
      )
    ))
  }
  if (isTRUE(plan$script_ready)) {
    variables <- if (isTRUE(plan$variable_filter_active)) {
      paste0(length(plan$variables), " source variable(s)")
    } else {
      paste0("all ", length(plan$variables), " available source variable(s)")
    }
    discovery_notice <- if (isTRUE(plan$group_discovery$active)) {
      shiny::tags$div(
        class = "mt-1",
        shiny::icon("magnifying-glass"),
        sprintf(
          paste0(
            " File-group fields match %d of %d eligible groups across ",
            "%d dataset(s); %d group(s) are excluded by those fields."
          ),
          plan$group_discovery$included_count,
          plan$group_discovery$candidate_count,
          plan$group_discovery$dataset_count,
          plan$group_discovery$excluded_count
        )
      )
    } else {
      NULL
    }
    group_filter_notice <- if (isTRUE(plan$group_filter$active)) {
      file_group_label <- if (
        identical(plan$group_filter$candidate_count, 1L)
      ) {
        "file group"
      } else {
        "file groups"
      }
      excluded_verb <- if (identical(plan$group_filter$excluded_count, 1L)) {
        "is"
      } else {
        "are"
      }
      shiny::tags$div(
        class = "mt-1",
        shiny::icon("filter"),
        sprintf(
          paste0(
            " Variable filters include %d of %d eligible %s; ",
            "%d %s excluded automatically because they are not compatible ",
            "with this selection."
          ),
          plan$group_filter$included_count,
          plan$group_filter$candidate_count,
          file_group_label,
          plan$group_filter$excluded_count,
          excluded_verb
        )
      )
    } else {
      NULL
    }
    return(shiny::tags$div(
      class = "alert alert-success py-2",
      role = "status",
      shiny::icon("circle-check"),
      paste0(
        " Ready to preview and export: ",
        length(plan$datasets),
        " dataset(s), ",
        length(plan$file_groups),
        " file group(s), and ",
        variables,
        "."
      ),
      discovery_notice,
      group_filter_notice
    ))
  }
  issues <- unique(c(plan$issues, plan$script_issues))
  compatibility_help <- if (length(plan$compatibility$issues) > 0L) {
    shiny::tags$p(
      class = "mb-1 mt-1",
      paste0(
        "The current file groups cannot be collected together. In Data ",
        "selection, choose a compatible subset of file groups or narrow ",
        "the datasets."
      )
    )
  } else {
    NULL
  }
  shiny::tags$div(
    class = "alert alert-warning py-2",
    role = "status",
    shiny::icon("triangle-exclamation"),
    shiny::tags$strong(" Selection needs attention"),
    compatibility_help,
    shiny::tags$ul(
      class = "mb-0 mt-1",
      lapply(issues, shiny::tags$li)
    )
  )
}

glc_explorer_handoff_information_ui <- function(ns) {
  data_only <- function(...) {
    shiny::conditionalPanel(
      condition = "input.handoff_mode === 'data'",
      ...,
      ns = ns
    )
  }
  metadata_only <- function(...) {
    shiny::conditionalPanel(
      condition = "input.handoff_mode === 'metadata'",
      ...,
      ns = ns
    )
  }
  step <- function(value, ...) {
    shiny::conditionalPanel(
      condition = sprintf("input.handoff_tab === '%s'", value),
      ...,
      ns = ns
    )
  }

  shiny::tags$aside(
    class = "handoff-information-column",
    `aria-label` = "Selection information",
    shiny::uiOutput(ns("plan_status")),
    step(
      "package",
      shiny::uiOutput(ns("package_source")),
      shiny::tags$div(
        class = "alert alert-info py-2 small",
        role = "note",
        paste0(
          "Choose a lightweight package-and-metadata handoff or continue ",
          "with a data import. Every export pins the validated revision."
        )
      )
    ),
    step(
      "groups",
      data_only(
        shiny::uiOutput(ns("seed_notice")),
        shiny::tags$div(
          class = "alert alert-light border py-2 small",
          role = "status",
          shiny::textOutput(ns("file_group_filter_count"), inline = TRUE)
        ),
        shiny::tags$p(
          class = "small text-body-secondary",
          paste0(
            "Exact group choices stay selected when a later participant, ",
            "device, or variable filter temporarily excludes them."
          )
        )
      )
    ),
    step(
      "people",
      data_only(
        shiny::uiOutput(ns("participant_device_step_note")),
        shiny::tags$div(
          class = "alert alert-info py-2 small",
          role = "note",
          paste0(
            "These filters narrow the active result without rewriting the ",
            "dataset or exact file-group choices from Step 2. Resetting a ",
            "filter restores the matching part of that selection."
          )
        )
      )
    ),
    step(
      "variables",
      data_only(
        shiny::uiOutput(ns("variable_step_note")),
        shiny::tags$div(
          class = "alert alert-info py-2 small",
          role = "note",
          paste0(
            "Variable filters may temporarily exclude file groups, but the ",
            "underlying Step 2 selection remains available when filters are ",
            "cleared."
          )
        )
      )
    ),
    step(
      "summary",
      shiny::tags$div(
        class = "alert alert-info py-2 small",
        role = "note",
        paste0(
          "Open the review sections beside this information column to inspect ",
          "the resolved selection and included file groups."
        )
      )
    ),
    step(
      "preview",
      metadata_only(
        shiny::tags$div(
          class = "alert alert-info py-2",
          role = "note",
          shiny::icon("circle-info"),
          paste0(
            " A data preview is not needed for a package-and-metadata ",
            "handoff. Continue directly to Export to R."
          )
        )
      ),
      data_only(
        shiny::uiOutput(ns("transfer_note")),
        shiny::uiOutput(ns("preview_status"))
      )
    ),
    step(
      "export",
      shiny::uiOutput(ns("script_note"))
    )
  )
}

selection_handoff_ui <- function(id) {
  ns <- shiny::NS(id)
  multi_select <- function(input_id, label, placeholder) {
    shiny::selectizeInput(
      ns(input_id),
      label,
      choices = character(),
      selected = character(),
      multiple = TRUE,
      options = list(
        placeholder = placeholder,
        searchField = c("text", "value")
      )
    )
  }
  data_only <- function(...) {
    shiny::conditionalPanel(
      condition = "input.handoff_mode === 'data'",
      ...,
      ns = ns
    )
  }
  metadata_only <- function(...) {
    shiny::conditionalPanel(
      condition = "input.handoff_mode === 'metadata'",
      ...,
      ns = ns
    )
  }
  control_block <- function(..., class = NULL) {
    shiny::tags$div(
      class = paste(c("handoff-control-block", class), collapse = " "),
      ...
    )
  }
  nav_button <- function(input_id, label, direction = c("forward", "back")) {
    direction <- match.arg(direction)
    shiny::actionButton(
      ns(input_id),
      label,
      icon = shiny::icon(
        if (identical(direction, "back")) {
          "arrow-left"
        } else {
          "arrow-right"
        }
      ),
      class = paste(
        "handoff-nav-action",
        if (identical(direction, "back")) {
          "btn-outline-primary"
        } else {
          "btn-primary"
        }
      )
    )
  }
  step_nav <- function(back = NULL, forward = NULL) {
    shiny::tags$nav(
      class = "handoff-step-nav",
      `aria-label` = "Wizard step navigation",
      shiny::tags$div(class = "handoff-step-nav-slot", back),
      shiny::tags$div(class = "handoff-step-nav-slot", forward)
    )
  }
  step_stack <- function(..., navigation) {
    shiny::tags$div(
      class = "handoff-step-stack",
      shiny::tags$div(class = "handoff-step-content", ...),
      navigation
    )
  }

  ui <- shiny::tags$div(
    class = "handoff-wizard",
    shiny::tags$div(
      class = "handoff-wizard-layout",
      shiny::tags$div(
        class = "handoff-control-column",
        bslib::navset_card_tab(
          id = ns("handoff_tab"),
          selected = "package",
          bslib::nav_panel(
            "1. Package & metadata",
            shiny::tags$div(
              class = "handoff-wizard-step handoff-wizard-step-form",
              step_stack(
                shiny::tags$div(
                  class = "handoff-control-grid",
                  control_block(
                    shiny::radioButtons(
                      ns("handoff_mode"),
                      "What should the script load?",
                      choices = c(
                        "Package and metadata only" = "metadata",
                        "Import matching data" = "data"
                      ),
                      selected = "metadata"
                    )
                  ),
                  metadata_only(
                    control_block(
                      multi_select(
                        "metadata_resources",
                        "Metadata resources",
                        "Choose metadata resources"
                      ),
                      shiny::helpText(
                        paste0(
                          "The default selects all core resources. No ",
                          "measurement data are imported."
                        )
                      )
                    )
                  )
                ),
                navigation = shiny::tags$div(
                  class = "handoff-step-nav-region",
                  metadata_only(
                    step_nav(
                      forward = nav_button(
                        "metadata_review",
                        "Review metadata export"
                      )
                    )
                  ),
                  data_only(
                    step_nav(
                      forward = nav_button(
                        "step_to_groups",
                        "Choose file groups"
                      )
                    )
                  )
                )
              )
            ),
            value = "package"
          ),
          bslib::nav_panel(
            "2. File groups",
            shiny::tags$div(
              class = "handoff-wizard-step handoff-wizard-step-form",
              data_only(
                step_stack(
                  shiny::tags$div(
                    class = "handoff-control-grid",
                    control_block(
                      shiny::tags$div(
                        class = "handoff-bounded-selectize",
                        multi_select(
                          "dataset_ids",
                          "Datasets (required)",
                          "Choose one or more datasets"
                        )
                      ),
                      shiny::tags$div(
                        class = "d-flex flex-wrap gap-2 mb-3",
                        shiny::actionButton(
                          ns("dataset_select_all"),
                          "Select all eligible",
                          icon = shiny::icon("check-double"),
                          class = "btn-sm"
                        ),
                        shiny::actionButton(
                          ns("dataset_clear"),
                          "Clear",
                          icon = shiny::icon("xmark"),
                          class = "btn-sm"
                        )
                      ),
                      shiny::actionButton(
                        ns("review_file_groups"),
                        "Filter file groups in Package contents",
                        icon = shiny::icon("table-list"),
                        class = "btn-sm btn-outline-primary mb-3"
                      )
                    ),
                    control_block(
                      multi_select("group_device_id", "Device", "Any device"),
                      multi_select(
                        "group_device_location",
                        "Wearing position",
                        "Any wearing position"
                      ),
                      multi_select(
                        "group_location_type",
                        "Position type",
                        "Any position type"
                      )
                    ),
                    control_block(
                      multi_select(
                        "group_modality",
                        "Modality",
                        "Any modality"
                      ),
                      multi_select("group_role", "Role", "Any role"),
                      multi_select("group_state", "Data state", "Any state")
                    ),
                    control_block(
                      multi_select(
                        "group_variable",
                        "Contains variables",
                        "Any variables"
                      ),
                      multi_select(
                        "group_term",
                        "Contains semantic terms",
                        "Any terms"
                      )
                    ),
                    control_block(
                      class = "handoff-control-span",
                      shiny::tags$div(
                        class = "handoff-bounded-selectize",
                        multi_select(
                          "file_group_ids",
                          "Exact file groups (advanced)",
                          "Every matching file group"
                        )
                      ),
                      shiny::tags$div(
                        class = "d-flex flex-wrap gap-2",
                        shiny::actionButton(
                          ns("file_groups_use_all"),
                          "Use every matching group",
                          icon = shiny::icon("layer-group"),
                          class = "btn-sm"
                        ),
                        shiny::actionButton(
                          ns("file_group_filters_clear"),
                          "Clear group filters",
                          icon = shiny::icon("xmark"),
                          class = "btn-sm"
                        )
                      )
                    )
                  ),
                  shiny::helpText(
                    paste0(
                      "Choices within a field use OR and fields combine with ",
                      "AND. Several variables or terms must all occur."
                    )
                  ),
                  navigation = step_nav(
                    back = nav_button(
                      "groups_back",
                      "Package & metadata",
                      direction = "back"
                    ),
                    forward = nav_button(
                      "step_to_people",
                      "Narrow people & devices"
                    )
                  )
                )
              )
            ),
            value = "groups"
          ),
          bslib::nav_panel(
            "3. Participants & devices",
            shiny::tags$div(
              class = "handoff-wizard-step handoff-wizard-step-form",
              data_only(
                step_stack(
                  shiny::tags$div(
                    class = "handoff-control-grid",
                    control_block(
                      shiny::tags$h6("Participants"),
                      shiny::uiOutput(ns("participant_age_filter")),
                      multi_select("participant_sex", "Sex", "Any sex"),
                      multi_select(
                        "participant_gender",
                        "Gender",
                        "Any gender"
                      ),
                      shiny::selectizeInput(
                        ns("characteristic_name"),
                        "Characteristic",
                        choices = c("No characteristic filter" = ""),
                        selected = "",
                        multiple = FALSE
                      ),
                      shiny::uiOutput(ns("characteristic_value_filter")),
                      multi_select(
                        "participant_ids",
                        "Participant IDs",
                        "All matching participants"
                      )
                    ),
                    control_block(
                      shiny::tags$h6("Devices"),
                      multi_select(
                        "device_manufacturer",
                        "Manufacturer",
                        "Any manufacturer"
                      ),
                      multi_select("device_model", "Model", "Any model"),
                      multi_select(
                        "device_sensor_type",
                        "Sensor type",
                        "Any sensor type"
                      ),
                      multi_select(
                        "device_ids",
                        "Device IDs",
                        "All matching devices"
                      )
                    )
                  ),
                  shiny::helpText(
                    paste0(
                      "Choices within a filter use OR; active filters combine ",
                      "with AND."
                    )
                  ),
                  navigation = step_nav(
                    back = nav_button(
                      "people_back",
                      "File groups",
                      direction = "back"
                    ),
                    forward = nav_button(
                      "step_to_variables",
                      "Narrow variables & rows"
                    )
                  )
                )
              )
            ),
            value = "people"
          ),
          bslib::nav_panel(
            "4. Variables & rows",
            shiny::tags$div(
              class = "handoff-wizard-step handoff-wizard-step-form",
              data_only(
                step_stack(
                  shiny::tags$div(
                    class = "handoff-control-grid",
                    control_block(
                      multi_select(
                        "variable_terms",
                        "Semantic terms (optional)",
                        "All semantic terms"
                      ),
                      multi_select(
                        "variables",
                        "Source variables (optional)",
                        "All source variables"
                      ),
                      shiny::helpText(
                        paste0(
                          "Leave both empty to import every source variable. ",
                          "Names or terms exclude incompatible groups."
                        )
                      ),
                      shiny::tags$div(
                        class = "d-flex flex-wrap gap-2 mb-3",
                        shiny::actionButton(
                          ns("variables_primary"),
                          "Primary",
                          icon = shiny::icon("star"),
                          class = "btn-sm"
                        ),
                        shiny::actionButton(
                          ns("variables_clear"),
                          "Use all variables",
                          icon = shiny::icon("asterisk"),
                          class = "btn-sm"
                        )
                      ),
                      shiny::helpText(
                        paste0(
                          "Primary uses declared primary variables, or all ",
                          "variables when none are declared."
                        )
                      )
                    ),
                    control_block(
                      shiny::radioButtons(
                        ns("row_limit_mode"),
                        "Rows to import",
                        choices = c(
                          "All rows" = "all",
                          "Maximum rows per file" = "limit"
                        ),
                        selected = "all"
                      ),
                      shiny::conditionalPanel(
                        condition = "input.row_limit_mode === 'limit'",
                        shiny::numericInput(
                          ns("max_rows_per_file"),
                          "Maximum rows per file",
                          value = 10000L,
                          min = 1L,
                          step = 1L,
                          width = "100%"
                        ),
                        ns = ns
                      ),
                      shiny::radioButtons(
                        ns("standardization"),
                        "Collection mode",
                        choices = c(
                          "LightLogR-compatible" = "lightlogr",
                          "Keep source columns" = "none"
                        ),
                        selected = "lightlogr"
                      )
                    )
                  ),
                  navigation = step_nav(
                    back = nav_button(
                      "variables_back",
                      "Participants & devices",
                      direction = "back"
                    ),
                    forward = nav_button(
                      "step_to_review",
                      "Review selection"
                    )
                  )
                )
              )
            ),
            value = "variables"
          ),
          bslib::nav_panel(
            "5. Review",
            shiny::tags$div(
              class = "handoff-wizard-step handoff-wizard-step-form",
              step_stack(
                shiny::tags$p(
                  class = "text-body-secondary",
                  paste0(
                    "Review the resolved selection below. Open only the ",
                    "section you need."
                  )
                ),
                bslib::accordion(
                  id = ns("review_sections"),
                  open = "selection",
                  multiple = FALSE,
                  class = "handoff-review-accordion",
                  bslib::accordion_panel(
                    "Selection summary",
                    shiny::tags$div(
                      class = "table-responsive",
                      shiny::tableOutput(ns("selection_summary"))
                    ),
                    value = "selection",
                    icon = shiny::icon("list-check")
                  ),
                  bslib::accordion_panel(
                    "Included file groups",
                    shiny::tags$div(
                      class = paste(
                        "d-flex flex-wrap align-items-center",
                        "justify-content-between gap-2 mb-2"
                      ),
                      shiny::tags$p(
                        class = "small text-body-secondary mb-0",
                        shiny::textOutput(
                          ns("group_summary_count"),
                          inline = TRUE
                        )
                      ),
                      shiny::uiOutput(ns("group_summary_pagination"))
                    ),
                    shiny::tags$div(
                      class = "table-responsive",
                      shiny::tableOutput(ns("group_summary"))
                    ),
                    value = "groups",
                    icon = shiny::icon("layer-group")
                  )
                ),
                data_only(shiny::uiOutput(ns("metadata_notice"))),
                navigation = step_nav(
                  back = nav_button(
                    "summary_back",
                    "Variables & rows",
                    direction = "back"
                  ),
                  forward = shiny::uiOutput(ns("summary_continue_ui"))
                )
              )
            ),
            value = "summary"
          ),
          bslib::nav_panel(
            "6. Preview",
            shiny::tags$div(
              class = "handoff-wizard-step handoff-wizard-step-form",
              metadata_only(
                step_stack(
                  navigation = step_nav(
                    back = nav_button(
                      "preview_metadata_back",
                      "Review",
                      direction = "back"
                    ),
                    forward = nav_button(
                      "metadata_preview_to_export",
                      "Continue to Export to R"
                    )
                  )
                )
              ),
              data_only(
                step_stack(
                  shiny::tags$div(
                    class = "handoff-control-grid handoff-preview-controls",
                    control_block(
                      shiny::numericInput(
                        ns("preview_files"),
                        "Files to preview",
                        value = 2L,
                        min = 1L,
                        max = 10000L,
                        step = 1L,
                        width = "100%"
                      )
                    ),
                    control_block(
                      shiny::numericInput(
                        ns("preview_rows"),
                        "Preview rows per file",
                        value = 10L,
                        min = 1L,
                        max = 1000L,
                        step = 1L,
                        width = "100%"
                      )
                    ),
                    control_block(
                      class = "handoff-control-span handoff-preview-build",
                      shiny::uiOutput(ns("preview_action"))
                    )
                  ),
                  shiny::tags$div(
                    class = "handoff-inline-result table-responsive mt-3",
                    shiny::tableOutput(ns("preview_table"))
                  ),
                  navigation = step_nav(
                    back = nav_button(
                      "preview_data_back",
                      "Review",
                      direction = "back"
                    ),
                    forward = nav_button(
                      "preview_continue",
                      "Continue to Export to R"
                    )
                  )
                )
              )
            ),
            value = "preview"
          ),
          bslib::nav_panel(
            "7. Export to R",
            shiny::tags$div(
              class = "handoff-wizard-step handoff-wizard-step-form",
              step_stack(
                shiny::tags$p(
                  class = "text-body-secondary",
                  paste0(
                    "Download the reproducible script or inspect it below ",
                    "before saving."
                  )
                ),
                shiny::uiOutput(ns("script_download_ui")),
                shiny::tags$div(
                  class = "handoff-inline-result handoff-script-result",
                  shiny::verbatimTextOutput(ns("script"), placeholder = TRUE)
                ),
                navigation = shiny::tags$div(
                  class = "handoff-step-nav-region",
                  metadata_only(
                    step_nav(
                      back = nav_button(
                        "export_metadata_back",
                        "Review",
                        direction = "back"
                      )
                    )
                  ),
                  data_only(
                    step_nav(
                      back = nav_button(
                        "export_data_back",
                        "Preview",
                        direction = "back"
                      )
                    )
                  )
                )
              )
            ),
            value = "export"
          )
        )
      ),
      glc_explorer_handoff_information_ui(ns)
    )
  )
  ui <- bslib::as_fill_carrier(ui)
  bslib::as_fill_item(
    ui,
    css_selector = ".handoff-wizard-layout"
  )
}

selection_handoff_server <- function(
  id,
  package,
  active,
  preselection = NULL,
  default_mode = "data",
  load_selection = glc_explorer_load_selection,
  preview_selection = glc_explorer_preview_selection,
  schedule_after_flush = glc_explorer_after_flush,
  select_nav = bslib::nav_select,
  show_modal = shiny::showModal
) {
  if (!shiny::is.reactive(package)) {
    glc_abort("{.arg package} must be a reactive expression.")
  }
  if (!shiny::is.reactive(active)) {
    glc_abort("{.arg active} must be a reactive expression.")
  }
  if (!is.null(preselection) && !shiny::is.reactive(preselection)) {
    glc_abort("{.arg preselection} must be a reactive expression or NULL.")
  }
  default_mode <- glc_explorer_handoff_mode(default_mode)

  shiny::moduleServer(id, function(input, output, session) {
    if (is.null(preselection)) {
      preselection <- shiny::reactive(NULL)
    }
    selection <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(glc_explorer_status(
      "Open a package before building a selection.",
      "empty"
    ))
    preview <- shiny::reactiveVal(NULL)
    preview_status <- shiny::reactiveVal(glc_explorer_status(
      "Choose a compatible selection, then build a preview.",
      "ready"
    ))
    loaded_key <- NULL
    observed_package_key <- NULL
    request_id <- 0L
    preview_request_id <- 0L
    preview_file_preference <- 2L
    preview_file_update <- NULL
    group_summary_page_number <- shiny::reactiveVal(1L)
    pending_preselection <- shiny::reactiveVal(NULL)
    applied_preselection <- shiny::reactiveVal(NULL)
    applied_preselection_id <- NULL
    contents_request <- shiny::reactiveVal(NULL)
    contents_request_id <- 0L

    age_slider_spec <- shiny::reactive({
      value <- selection()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_age_slider_spec(value$participants$age)
    })

    characteristic_filter_spec <- shiny::reactive({
      value <- selection()
      if (is.null(value)) {
        return(glc_explorer_characteristic_filter_spec(
          tibble::tibble(),
          character()
        ))
      }
      glc_explorer_characteristic_filter_spec(
        value$participant_characteristics,
        input$characteristic_name
      )
    })

    output$participant_age_filter <- shiny::renderUI({
      spec <- age_slider_spec()
      if (is.null(spec)) {
        return(shiny::tags$div(
          class = "mb-3",
          shiny::tags$div(class = "form-label", "Age range"),
          shiny::tags$p(
            class = "text-body-secondary mb-0",
            "No numeric participant ages are available."
          )
        ))
      }
      shiny::tagList(
        shiny::sliderInput(
          session$ns("participant_age"),
          "Age range",
          min = spec$min,
          max = spec$max,
          value = spec$value,
          step = spec$step,
          width = "100%",
          dragRange = TRUE
        ),
        shiny::helpText(
          "Move either handle to include participants within that age range."
        )
      )
    })

    output$characteristic_value_filter <- shiny::renderUI({
      spec <- characteristic_filter_spec()
      selected <- shiny::isolate(
        input$characteristic_values %||% character()
      )
      if (identical(spec$type, "numeric") && !is.null(spec$slider)) {
        selected_range <- glc_explorer_numeric_age_range(selected)
        if (length(selected_range) == 0L) {
          selected_range <- spec$slider$value
        } else {
          selected_range <- c(
            max(spec$slider$min, selected_range[[1L]]),
            min(spec$slider$max, selected_range[[2L]])
          )
          if (selected_range[[1L]] > selected_range[[2L]]) {
            selected_range <- spec$slider$value
          }
        }
        return(shiny::tagList(
          shiny::sliderInput(
            session$ns("characteristic_values"),
            "Characteristic value range",
            min = spec$slider$min,
            max = spec$slider$max,
            value = selected_range,
            step = spec$slider$step,
            width = "100%",
            dragRange = TRUE
          ),
          shiny::helpText(
            paste0(
              "Move either handle to include participants whose ",
              spec$name,
              " value falls within that range."
            )
          )
        ))
      }
      choices <- glc_explorer_choice_values(spec$values)
      shiny::selectizeInput(
        session$ns("characteristic_values"),
        "Characteristic values",
        choices = choices,
        selected = intersect(
          glc_explorer_nonempty_values(selected),
          unname(choices)
        ),
        multiple = TRUE,
        options = list(
          placeholder = "Any value",
          searchField = c("text", "value")
        )
      )
    })

    update_choices <- function(
      id,
      choices,
      selected = character(),
      universe = choices
    ) {
      choices <- glc_explorer_preserve_selected_choices(
        choices,
        selected,
        universe
      )
      server_side <- length(unname(choices)) > 100L
      shiny::updateSelectizeInput(
        session,
        id,
        choices = choices,
        selected = intersect(
          glc_explorer_nonempty_values(selected),
          unname(choices)
        ),
        server = server_side
      )
    }
    update_multi <- function(id, values, selected = character()) {
      update_choices(
        id,
        glc_explorer_choice_values(values),
        selected
      )
    }

    clear_inputs <- function() {
      for (id in c(
        "participant_sex",
        "participant_gender",
        "participant_ids",
        "device_manufacturer",
        "device_model",
        "device_sensor_type",
        "device_ids",
        "dataset_ids",
        "group_device_id",
        "group_device_location",
        "group_location_type",
        "group_modality",
        "group_role",
        "group_state",
        "group_variable",
        "group_term",
        "file_group_ids",
        "variable_terms",
        "variables"
      )) {
        shiny::updateSelectizeInput(
          session,
          id,
          choices = character(),
          selected = character()
        )
      }
      shiny::updateSelectizeInput(
        session,
        "characteristic_name",
        choices = c("No characteristic filter" = ""),
        selected = ""
      )
      shiny::updateRadioButtons(
        session,
        "standardization",
        selected = "lightlogr"
      )
      shiny::updateRadioButtons(
        session,
        "row_limit_mode",
        selected = "all"
      )
      shiny::updateNumericInput(
        session,
        "max_rows_per_file",
        value = 10000L
      )
      shiny::updateNumericInput(session, "preview_files", value = 2L)
      shiny::updateNumericInput(session, "preview_rows", value = 10L)
    }

    handoff_mode <- shiny::reactive({
      glc_explorer_handoff_mode(input$handoff_mode %||% default_mode)
    })

    metadata_resource_inventory <- shiny::reactive({
      value <- package()
      if (is.null(value)) {
        return(NULL)
      }
      glc_resources(value)
    })

    shiny::observeEvent(
      metadata_resource_inventory(),
      {
        inventory <- metadata_resource_inventory()
        if (is.null(inventory)) {
          return()
        }
        choices <- stats::setNames(inventory$resource, inventory$resource)
        selected <- unique(inventory$resource[inventory$core %in% TRUE])
        update_choices("metadata_resources", choices, selected)
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    output$package_source <- shiny::renderUI({
      value <- package()
      if (is.null(value)) {
        return(shiny::tags$p(
          class = "small text-body-secondary",
          "Open a package from the Registry first."
        ))
      }
      info <- glc_explorer_package_selection_info(value)
      shiny::tags$div(
        class = "alert alert-light border py-2 small",
        shiny::tags$div(
          shiny::tags$strong("Repository: "),
          shiny::tags$code(info$repository)
        ),
        shiny::tags$div(
          shiny::tags$strong("Exact revision: "),
          shiny::tags$code(substr(info$commit, 1L, 12L))
        )
      )
    })

    normalize_preselection <- function(value) {
      required <- c(
        "package_key",
        "request_id",
        "dataset_ids",
        "file_group_ids"
      )
      if (!is.list(value) || !all(required %in% names(value))) {
        return(NULL)
      }
      list(
        package_key = as.character(value$package_key[[1L]]),
        request_id = as.character(value$request_id[[1L]]),
        dataset_ids = glc_explorer_nonempty_values(value$dataset_ids),
        file_group_ids = glc_explorer_nonempty_values(value$file_group_ids)
      )
    }

    apply_pending_preselection <- function() {
      request <- shiny::isolate(pending_preselection())
      value <- shiny::isolate(selection())
      package_value <- shiny::isolate(package())
      if (is.null(request) || is.null(value) || is.null(package_value)) {
        return(invisible(FALSE))
      }
      if (
        !identical(
          request$package_key,
          glc_explorer_package_key(package_value)
        )
      ) {
        pending_preselection(NULL)
        applied_preselection(NULL)
        return(invisible(FALSE))
      }
      groups <- value$groups[
        value$groups$file_group_id %in% request$file_group_ids,
        ,
        drop = FALSE
      ]
      file_group_ids <- unique(groups$file_group_id)
      dataset_ids <- unique(groups$dataset_id)
      if (length(file_group_ids) == 0L) {
        pending_preselection(NULL)
        applied_preselection(NULL)
        status(glc_explorer_status(
          "The transferred file-group selection is not available in this package.",
          "error"
        ))
        return(invisible(FALSE))
      }

      for (id in c(
        "participant_sex",
        "participant_gender",
        "participant_ids",
        "device_manufacturer",
        "device_model",
        "device_sensor_type",
        "device_ids",
        "group_device_id",
        "group_device_location",
        "group_location_type",
        "group_modality",
        "group_role",
        "group_state",
        "group_variable",
        "group_term",
        "variable_terms",
        "variables"
      )) {
        shiny::updateSelectizeInput(session, id, selected = character())
      }
      shiny::updateSelectizeInput(
        session,
        "characteristic_name",
        selected = ""
      )
      update_multi(
        "dataset_ids",
        value$datasets$dataset_id,
        dataset_ids
      )
      update_choices(
        "file_group_ids",
        glc_explorer_file_group_choices(groups),
        file_group_ids
      )
      pending_preselection(NULL)
      applied_preselection_id <<- request$request_id
      applied_preselection(list(
        request_id = request$request_id,
        requested_group_count = length(request$file_group_ids),
        file_group_ids = file_group_ids,
        dataset_ids = dataset_ids
      ))
      status(glc_explorer_status(
        sprintf(
          "Transferred %d file group(s) across %d dataset(s) from Package contents.",
          length(file_group_ids),
          length(dataset_ids)
        ),
        "success"
      ))
      select_nav("handoff_tab", selected = "groups", session = session)
      invisible(TRUE)
    }

    schedule_pending_preselection <- function() {
      request <- shiny::isolate(pending_preselection())
      if (is.null(request)) {
        return(invisible(FALSE))
      }
      scheduling_error <- tryCatch(
        {
          schedule_after_flush(
            apply_pending_preselection,
            session = session
          )
          NULL
        },
        error = identity
      )
      if (inherits(scheduling_error, "error")) {
        status(glc_explorer_status(
          paste(
            "Could not apply the transferred file-group selection:",
            conditionMessage(scheduling_error)
          ),
          "error"
        ))
        return(invisible(FALSE))
      }
      invisible(TRUE)
    }

    shiny::observeEvent(
      preselection(),
      {
        request <- normalize_preselection(preselection())
        if (
          is.null(request) ||
            identical(request$request_id, applied_preselection_id)
        ) {
          return()
        }
        current_key <- glc_explorer_package_key(package())
        if (
          is.null(current_key) || !identical(request$package_key, current_key)
        ) {
          return()
        }
        pending_preselection(request)
        shiny::updateRadioButtons(
          session,
          "handoff_mode",
          selected = "data"
        )
        apply_pending_preselection()
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    finish_load <- function(value, key, request) {
      if (!identical(request, request_id)) {
        return()
      }
      result <- tryCatch(load_selection(value), error = identity)
      if (inherits(result, "error")) {
        selection(NULL)
        status(glc_explorer_status(
          paste("Could not load selection data:", conditionMessage(result)),
          "error"
        ))
        return()
      }
      required <- c(
        "participants",
        "participant_characteristics",
        "devices",
        "datasets",
        "files",
        "variables",
        "groups"
      )
      valid <- is.list(result) && all(required %in% names(result))
      if (valid) {
        valid <- all(vapply(
          result[required],
          inherits,
          logical(1),
          what = "data.frame"
        ))
      }
      if (!valid) {
        selection(NULL)
        status(glc_explorer_status(
          "Could not load selection data: the result is incomplete.",
          "error"
        ))
        return()
      }

      selection(result)
      loaded_key <<- key
      status(glc_explorer_status(
        sprintf(
          paste0(
            "Loaded %d participants, %d devices, %d datasets, ",
            "and %d source variables."
          ),
          nrow(result$participants),
          nrow(result$devices),
          nrow(result$datasets),
          nrow(result$variables)
        ),
        "success"
      ))
      schedule_pending_preselection()
    }

    shiny::observe({
      value <- package()
      key <- glc_explorer_package_key(value)
      mode <- handoff_mode()
      if (is.null(value)) {
        request_id <<- request_id + 1L
        selection(NULL)
        loaded_key <<- NULL
        observed_package_key <<- NULL
        pending_preselection(NULL)
        applied_preselection(NULL)
        applied_preselection_id <<- NULL
        clear_inputs()
        shiny::updateRadioButtons(
          session,
          "handoff_mode",
          selected = default_mode
        )
        status(glc_explorer_status(
          "Open a package before building a selection.",
          "empty"
        ))
        return()
      }
      if (!identical(key, observed_package_key)) {
        request_id <<- request_id + 1L
        selection(NULL)
        loaded_key <<- NULL
        observed_package_key <<- key
        pending_preselection(NULL)
        applied_preselection(NULL)
        applied_preselection_id <<- NULL
        clear_inputs()
        shiny::updateRadioButtons(
          session,
          "handoff_mode",
          selected = default_mode
        )
        status(glc_explorer_status(
          if (isTRUE(active())) {
            "Package and metadata are ready for an R handoff."
          } else {
            "Open Select & hand off to build a package or data handoff."
          },
          if (isTRUE(active())) "success" else "ready"
        ))
        if (identical(default_mode, "metadata")) {
          return()
        }
      }
      if (!isTRUE(active())) {
        status(glc_explorer_status(
          "Open Select & hand off to build a package or data handoff.",
          "ready"
        ))
        return()
      }
      if (identical(mode, "metadata")) {
        status(glc_explorer_status(
          "Package and metadata are ready for an R handoff.",
          "success"
        ))
        return()
      }
      if (identical(key, loaded_key)) {
        status(glc_explorer_status(
          sprintf(
            paste0(
              "Loaded %d participants, %d devices, %d datasets, ",
              "and %d source variables."
            ),
            nrow(selection()$participants),
            nrow(selection()$devices),
            nrow(selection()$datasets),
            nrow(selection()$variables)
          ),
          "success"
        ))
        return()
      }

      request_id <<- request_id + 1L
      request <- request_id
      selection(NULL)
      clear_inputs()
      status(glc_explorer_status(
        paste(
          "Loading selection data:",
          "participants, devices, datasets, files, and variables\u2026"
        ),
        "loading"
      ))
      scheduling_error <- tryCatch(
        {
          schedule_after_flush(
            function() finish_load(value, key, request),
            session = session
          )
          NULL
        },
        error = identity
      )
      if (inherits(scheduling_error, "error")) {
        status(glc_explorer_status(
          paste(
            "Could not start loading selection metadata:",
            conditionMessage(scheduling_error)
          ),
          "error"
        ))
      }
    })

    shiny::observeEvent(
      selection(),
      {
        value <- selection()
        if (is.null(value)) {
          return()
        }
        update_multi("participant_sex", value$participants$sex)
        update_multi("participant_gender", value$participants$gender)
        update_multi(
          "dataset_ids",
          value$datasets$dataset_id,
          shiny::isolate(input$dataset_ids)
        )
        characteristic_names <- glc_explorer_choice_values(
          value$participant_characteristics$characteristic_name
        )
        shiny::updateSelectizeInput(
          session,
          "characteristic_name",
          choices = c("No characteristic filter" = "", characteristic_names),
          selected = ""
        )
        update_multi("device_manufacturer", value$devices$manufacturer)
        update_multi("device_model", value$devices$model)
        update_multi("device_sensor_type", value$devices$sensor_type)
      },
      ignoreNULL = TRUE
    )

    facets <- shiny::reactive({
      age_spec <- age_slider_spec()
      characteristic_spec <- characteristic_filter_spec()
      list(
        participant = list(
          age = glc_explorer_age_filter_value(
            input$participant_age,
            if (is.null(age_spec)) numeric() else age_spec$value
          ),
          sex = input$participant_sex %||% character(),
          gender = input$participant_gender %||% character(),
          characteristic_name = input$characteristic_name %||% character(),
          characteristic_values = glc_explorer_characteristic_filter_value(
            input$characteristic_values,
            characteristic_spec
          )
        ),
        device = list(
          manufacturer = input$device_manufacturer %||% character(),
          model = input$device_model %||% character(),
          sensor_type = input$device_sensor_type %||% character()
        )
      )
    })

    eligible_participants <- shiny::reactive({
      value <- selection()
      if (is.null(value)) {
        return(character())
      }
      glc_explorer_filter_participant_ids(
        value$participants,
        value$participant_characteristics,
        facets()$participant
      )
    })
    eligible_devices <- shiny::reactive({
      value <- selection()
      if (is.null(value)) {
        return(character())
      }
      glc_explorer_filter_device_ids(value$devices, facets()$device)
    })

    shiny::observeEvent(
      eligible_participants(),
      {
        value <- selection()
        if (is.null(value)) {
          return()
        }
        selected <- shiny::isolate(input$participant_ids)
        update_choices(
          "participant_ids",
          glc_explorer_choice_values(eligible_participants()),
          selected,
          glc_explorer_choice_values(value$participants$participant_id)
        )
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      eligible_devices(),
      {
        value <- selection()
        if (is.null(value)) {
          return()
        }
        selected <- shiny::isolate(input$device_ids)
        update_choices(
          "device_ids",
          glc_explorer_choice_values(eligible_devices()),
          selected,
          glc_explorer_choice_values(value$devices$device_id)
        )
      },
      ignoreInit = TRUE
    )

    scope <- shiny::reactive({
      value <- selection()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_selection_scope(
        value,
        facets(),
        participant_ids = input$participant_ids %||% character(),
        device_ids = input$device_ids %||% character()
      )
    })

    available_groups <- shiny::reactive({
      value <- selection()
      current_scope <- scope()
      if (is.null(value) || is.null(current_scope)) {
        return(NULL)
      }
      glc_explorer_resolved_group_rows(
        value,
        current_scope,
        input$dataset_ids %||% character()
      )
    })

    dataset_groups <- shiny::reactive({
      value <- selection()
      if (is.null(value)) {
        return(NULL)
      }
      dataset_ids <- glc_explorer_nonempty_values(input$dataset_ids)
      value$groups[
        value$groups$dataset_id %in% dataset_ids,
        ,
        drop = FALSE
      ]
    })

    group_filters <- shiny::reactive({
      glc_explorer_selection_group_filter_spec(
        device_ids = input$group_device_id %||% character(),
        device_locations = input$group_device_location %||% character(),
        location_types = input$group_location_type %||% character(),
        modalities = input$group_modality %||% character(),
        roles = input$group_role %||% character(),
        data_states = input$group_state %||% character(),
        variable_names = input$group_variable %||% character(),
        variable_terms = input$group_term %||% character()
      )
    })

    shiny::observeEvent(
      dataset_groups(),
      {
        value <- dataset_groups()
        all_groups <- selection()$groups
        if (is.null(value) || is.null(all_groups)) {
          return()
        }
        choices <- glc_explorer_selection_group_field_choices(value)
        all_choices <- glc_explorer_selection_group_field_choices(all_groups)
        input_choices <- list(
          group_device_id = choices$device_ids,
          group_device_location = choices$device_locations,
          group_location_type = choices$location_types,
          group_modality = choices$modalities,
          group_role = choices$roles,
          group_state = choices$data_states,
          group_variable = choices$variable_names,
          group_term = choices$variable_terms
        )
        universe_choices <- list(
          group_device_id = all_choices$device_ids,
          group_device_location = all_choices$device_locations,
          group_location_type = all_choices$location_types,
          group_modality = all_choices$modalities,
          group_role = all_choices$roles,
          group_state = all_choices$data_states,
          group_variable = all_choices$variable_names,
          group_term = all_choices$variable_terms
        )
        for (id in names(input_choices)) {
          selected <- shiny::isolate(input[[id]])
          update_choices(
            id,
            input_choices[[id]],
            selected,
            universe_choices[[id]]
          )
        }
      },
      ignoreInit = TRUE
    )

    group_filter_result <- shiny::reactive({
      value <- available_groups()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_selection_group_filter_result(value, group_filters())
    })

    matching_groups <- shiny::reactive({
      value <- group_filter_result()
      if (is.null(value)) {
        return(NULL)
      }
      value$groups
    })

    effective_file_group_ids <- shiny::reactive({
      value <- matching_groups()
      requested <- glc_explorer_nonempty_values(input$file_group_ids)
      if (is.null(value) || length(requested) == 0L) {
        return(character())
      }
      intersect(requested, value$file_group_id)
    })

    shiny::observeEvent(
      matching_groups(),
      {
        value <- matching_groups()
        if (is.null(value)) {
          return()
        }
        update_choices(
          "file_group_ids",
          glc_explorer_file_group_choices(value),
          shiny::isolate(input$file_group_ids),
          glc_explorer_file_group_choices(selection()$groups)
        )
      },
      ignoreInit = TRUE
    )

    available_variables <- shiny::reactive({
      value <- selection()
      groups <- matching_groups()
      if (is.null(value) || is.null(groups)) {
        return(NULL)
      }
      requested_groups <- effective_file_group_ids()
      selected_groups <- glc_explorer_nonempty_values(input$file_group_ids)
      if (length(selected_groups) > 0L) {
        groups <- groups[
          groups$file_group_id %in% requested_groups,
          ,
          drop = FALSE
        ]
      }
      value$variables[
        value$variables$file_group_id %in% groups$file_group_id,
        ,
        drop = FALSE
      ]
    })

    shiny::observeEvent(
      available_variables(),
      {
        value <- available_variables()
        if (is.null(value)) {
          return()
        }
        choices <- glc_explorer_variable_term_choices(value)
        selected <- glc_explorer_nonempty_values(
          shiny::isolate(input$variable_terms)
        )
        update_choices(
          "variable_terms",
          choices,
          selected,
          glc_explorer_variable_term_choices(selection()$variables)
        )
      },
      ignoreInit = TRUE
    )

    selectable_variables <- shiny::reactive({
      value <- available_variables()
      if (is.null(value)) {
        return(NULL)
      }
      terms <- glc_explorer_nonempty_values(input$variable_terms)
      if (length(terms) > 0L && "term" %in% names(value)) {
        value <- value[value$term %in% terms, , drop = FALSE]
      }
      value
    })

    shiny::observeEvent(
      selectable_variables(),
      {
        value <- selectable_variables()
        if (is.null(value)) {
          return()
        }
        choices <- unique(value$name)
        selected <- glc_explorer_nonempty_values(
          shiny::isolate(input$variables)
        )
        update_choices(
          "variables",
          glc_explorer_choice_values(choices),
          selected,
          glc_explorer_choice_values(selection()$variables$name)
        )
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$dataset_select_all,
      {
        value <- scope()
        if (!is.null(value)) {
          update_multi("dataset_ids", value$dataset_ids, value$dataset_ids)
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$dataset_clear,
      {
        shiny::updateSelectizeInput(
          session,
          "dataset_ids",
          selected = character()
        )
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$file_groups_use_all,
      {
        shiny::updateSelectizeInput(
          session,
          "file_group_ids",
          selected = character()
        )
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$file_group_filters_clear,
      {
        for (id in c(
          "group_device_id",
          "group_device_location",
          "group_location_type",
          "group_modality",
          "group_role",
          "group_state",
          "group_variable",
          "group_term",
          "file_group_ids"
        )) {
          shiny::updateSelectizeInput(
            session,
            id,
            selected = character()
          )
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$variables_primary,
      {
        value <- selectable_variables()
        if (!is.null(value)) {
          update_multi(
            "variables",
            unique(value$name),
            glc_explorer_default_variables(value)
          )
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$step_to_groups,
      select_nav("handoff_tab", selected = "groups", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$step_to_people,
      select_nav("handoff_tab", selected = "people", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$step_to_variables,
      select_nav("handoff_tab", selected = "variables", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$step_to_review,
      select_nav("handoff_tab", selected = "summary", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$groups_back,
      select_nav("handoff_tab", selected = "package", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$people_back,
      select_nav("handoff_tab", selected = "groups", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$variables_back,
      select_nav("handoff_tab", selected = "people", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$summary_back,
      select_nav("handoff_tab", selected = "variables", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$preview_metadata_back,
      select_nav("handoff_tab", selected = "summary", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$preview_data_back,
      select_nav("handoff_tab", selected = "summary", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$export_metadata_back,
      select_nav("handoff_tab", selected = "summary", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$export_data_back,
      select_nav("handoff_tab", selected = "preview", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$metadata_review,
      select_nav("handoff_tab", selected = "export", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$metadata_preview_to_export,
      select_nav("handoff_tab", selected = "export", session = session),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$review_file_groups,
      {
        contents_request_id <<- contents_request_id + 1L
        contents_request(list(
          request_id = contents_request_id,
          tab = "File groups",
          dataset_ids = glc_explorer_nonempty_values(input$dataset_ids)
        ))
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$variables_clear,
      {
        shiny::updateSelectizeInput(
          session,
          "variable_terms",
          selected = character()
        )
        shiny::updateSelectizeInput(
          session,
          "variables",
          selected = character()
        )
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$summary_continue,
      {
        selected <- if (identical(handoff_mode(), "metadata")) {
          "export"
        } else {
          "preview"
        }
        select_nav("handoff_tab", selected = selected, session = session)
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$preview_continue,
      {
        select_nav("handoff_tab", selected = "export", session = session)
      },
      ignoreInit = TRUE
    )

    plan <- shiny::reactive({
      package_value <- package()
      mode <- handoff_mode()
      if (is.null(package_value)) {
        return(NULL)
      }
      if (identical(mode, "metadata")) {
        return(glc_explorer_build_selection_plan(
          package_value,
          selection = NULL,
          facets = list(participant = list(), device = list()),
          mode = mode,
          metadata_resources = input$metadata_resources %||% character()
        ))
      }
      value <- selection()
      current_scope <- scope()
      current_group_discovery <- group_filter_result()
      if (
        is.null(value) ||
          is.null(current_scope) ||
          is.null(current_group_discovery)
      ) {
        return(NULL)
      }
      glc_explorer_build_selection_plan(
        package_value,
        value,
        facets(),
        mode = mode,
        participant_ids = input$participant_ids %||% character(),
        device_ids = input$device_ids %||% character(),
        dataset_ids = input$dataset_ids %||% character(),
        group_filters = group_filters(),
        file_group_ids = input$file_group_ids %||% character(),
        variables = input$variables %||% character(),
        terms = input$variable_terms %||% character(),
        n_max = if (identical(input$row_limit_mode %||% "all", "limit")) {
          input$max_rows_per_file %||% 10000L
        } else {
          Inf
        },
        standardize = input$standardization %||% "lightlogr",
        resolved_scope = current_scope,
        resolved_group_discovery = current_group_discovery
      )
    })

    shiny::observeEvent(
      plan()$preview_files,
      {
        available <- length(
          glc_explorer_nonempty_values(plan()$preview_files)
        )
        if (available < 1L) {
          return()
        }
        value <- min(preview_file_preference, available)
        preview_file_update <<- value
        shiny::updateNumericInput(
          session,
          "preview_files",
          value = value,
          min = 1L,
          max = available
        )
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    group_summary_page <- shiny::reactive({
      value <- selection()
      plan_value <- plan()
      if (is.null(value) || is.null(plan_value)) {
        return(NULL)
      }
      glc_explorer_inventory_page(
        glc_explorer_selection_group_table(value, plan_value),
        page = group_summary_page_number(),
        page_size = 100L
      )
    })

    shiny::observeEvent(
      plan()$file_groups,
      group_summary_page_number(1L),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$group_summary_previous,
      {
        page <- group_summary_page()
        if (!is.null(page)) {
          group_summary_page_number(max(1L, page$page - 1L))
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$group_summary_next,
      {
        page <- group_summary_page()
        if (!is.null(page)) {
          group_summary_page_number(min(page$page_count, page$page + 1L))
        }
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      plan(),
      {
        preview_request_id <<- preview_request_id + 1L
        preview(NULL)
        preview_status(glc_explorer_status(
          "Build a preview after the selection is ready.",
          "ready"
        ))
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$preview_rows,
      {
        preview_request_id <<- preview_request_id + 1L
        preview(NULL)
        preview_status(glc_explorer_status(
          "Build a new preview with the selected row limit.",
          "ready"
        ))
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$preview_files,
      {
        input_value <- suppressWarnings(as.integer(input$preview_files[[1L]]))
        if (!is.na(input_value)) {
          programmatic <- !is.null(preview_file_update) &&
            identical(input_value, preview_file_update)
          preview_file_update <<- NULL
          if (!programmatic) {
            preview_file_preference <<- max(1L, input_value)
          }
        }
        preview_request_id <<- preview_request_id + 1L
        preview(NULL)
        preview_status(glc_explorer_status(
          "Build a new preview with the selected file limit.",
          "ready"
        ))
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$build_preview,
      {
        value <- shiny::isolate(plan())
        package_value <- shiny::isolate(package())
        if (is.null(value) || !isTRUE(value$preview_ready)) {
          preview(NULL)
          issues <- value$issues %||% "The selection is not ready."
          preview_status(glc_explorer_status(
            paste("Preview blocked:", paste(issues, collapse = " ")),
            "error"
          ))
          return()
        }

        preview_request_id <<- preview_request_id + 1L
        request <- preview_request_id
        preview(NULL)
        file_limit <- glc_explorer_preview_file_limit(
          input$preview_files,
          length(value$preview_files)
        )
        preview_status(glc_explorer_status(
          sprintf(
            "Reading %d preview file(s)\u2026",
            file_limit
          ),
          "loading"
        ))
        row_limit <- glc_explorer_preview_row_limit(input$preview_rows)
        scheduling_error <- tryCatch(
          {
            schedule_after_flush(
              function() {
                result <- tryCatch(
                  preview_selection(
                    package_value,
                    value,
                    row_limit,
                    file_limit
                  ),
                  error = identity
                )
                if (!identical(request, preview_request_id)) {
                  return()
                }
                if (inherits(result, "error")) {
                  preview(NULL)
                  preview_status(glc_explorer_status(
                    paste(
                      "Could not build the preview:",
                      conditionMessage(result)
                    ),
                    "error"
                  ))
                  return()
                }
                preview(result)
                preview_status(glc_explorer_status(
                  sprintf(
                    paste0(
                      "Preview ready: showing %d collected row(s), with a ",
                      "limit of %d row(s) per file across %d preview file(s)."
                    ),
                    nrow(result),
                    row_limit,
                    file_limit
                  ),
                  "success"
                ))
              },
              session = session
            )
            NULL
          },
          error = identity
        )
        if (inherits(scheduling_error, "error")) {
          preview_status(glc_explorer_status(
            paste(
              "Could not start the preview:",
              conditionMessage(scheduling_error)
            ),
            "error"
          ))
        }
      },
      ignoreInit = TRUE
    )

    output$file_group_filter_count <- shiny::renderText({
      value <- group_filter_result()
      if (is.null(value) || identical(value$candidate_count, 0L)) {
        return("Select one or more datasets to discover their file groups.")
      }
      sprintf(
        "%d of %d eligible file groups match across %d dataset(s).",
        value$included_count,
        value$candidate_count,
        length(unique(value$groups$dataset_id))
      )
    })
    output$seed_notice <- shiny::renderUI({
      value <- applied_preselection()
      if (is.null(value)) {
        return(shiny::tags$div(
          class = "alert alert-light border py-2 small",
          role = "note",
          paste0(
            "No file-group preselection has been transferred. Choose ",
            "datasets here or filter groups in Package contents."
          )
        ))
      }
      dropped <- value$requested_group_count - length(value$file_group_ids)
      shiny::tags$div(
        class = "alert alert-success py-2 small",
        role = "status",
        shiny::icon("circle-check"),
        sprintf(
          " Seeded from Package contents: %d file group(s) across %d dataset(s).",
          length(value$file_group_ids),
          length(value$dataset_ids)
        ),
        if (dropped > 0L) {
          paste0(" ", dropped, " unavailable group(s) were omitted.")
        }
      )
    })
    output$participant_device_step_note <- shiny::renderUI({
      dataset_ids <- glc_explorer_nonempty_values(input$dataset_ids)
      if (length(dataset_ids) > 0L) {
        return(NULL)
      }
      shiny::tags$div(
        class = "alert alert-warning py-2 small",
        role = "note",
        "Choose datasets and file groups in Step 2 before narrowing people or devices."
      )
    })
    output$variable_step_note <- shiny::renderUI({
      groups <- matching_groups()
      if (!is.null(groups) && nrow(groups) > 0L) {
        return(NULL)
      }
      shiny::tags$div(
        class = "alert alert-warning py-2 small",
        role = "note",
        "Choose matching file groups first; variable and semantic-term choices are scoped to them."
      )
    })
    output$status_message <- shiny::renderUI({
      glc_explorer_status_tag(status())
    })
    output$plan_status <- shiny::renderUI({
      value <- plan()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_selection_issues_tag(value)
    })
    output$selection_summary <- shiny::renderTable(
      {
        value <- plan()
        if (is.null(value)) {
          return(NULL)
        }
        glc_explorer_selection_summary_table(value)
      },
      rownames = FALSE,
      bordered = FALSE,
      spacing = "m"
    )
    output$summary_continue_ui <- shiny::renderUI({
      value <- plan()
      if (is.null(value) || !isTRUE(value$script_ready)) {
        return(NULL)
      }
      metadata <- identical(value$mode %||% "data", "metadata")
      shiny::actionButton(
        session$ns("summary_continue"),
        if (metadata) "Continue to Export to R" else "Continue to preview",
        icon = shiny::icon("arrow-right"),
        class = "btn-primary handoff-nav-action"
      )
    })
    output$group_summary_count <- shiny::renderText({
      glc_explorer_inventory_page_message(
        group_summary_page(),
        "included file groups"
      )
    })
    output$group_summary_pagination <- shiny::renderUI({
      glc_explorer_inventory_pagination_tag(
        group_summary_page(),
        input_prefix = "group_summary",
        item = "included file groups",
        ns = session$ns
      )
    })
    output$group_summary <- shiny::renderTable(
      {
        page <- group_summary_page()
        if (is.null(page)) {
          return(NULL)
        }
        page$data
      },
      rownames = FALSE,
      bordered = FALSE,
      spacing = "m"
    )
    output$metadata_notice <- shiny::renderUI({
      value <- selection()
      issues <- value$metadata_issues %||% character()
      if (is.null(value) || length(issues) == 0L) {
        return(NULL)
      }
      shiny::tags$div(
        class = "alert alert-warning py-2",
        role = "note",
        shiny::tags$strong("Some optional metadata could not be loaded:"),
        shiny::tags$ul(class = "mb-0", lapply(issues, shiny::tags$li))
      )
    })
    output$transfer_note <- shiny::renderUI({
      value <- plan()
      if (is.null(value)) {
        return(NULL)
      }
      preview_limit <- glc_explorer_preview_row_limit(input$preview_rows)
      if (is.finite(value$n_max %||% Inf)) {
        preview_limit <- min(preview_limit, value$n_max)
      }
      transfer <- glc_explorer_preview_transfer(
        value,
        input$preview_files
      )
      shiny::tags$div(
        class = "alert alert-info py-2",
        role = "note",
        shiny::icon("circle-info"),
        paste0(
          " The preview reads at most ",
          preview_limit,
          " rows from ",
          length(transfer$files),
          " of ",
          transfer$available,
          " available preview file(s). Remote preview files must still be ",
          "materialized completely. Estimated preview transfer: ",
          glc_explorer_format_bytes(
            transfer$estimated_bytes,
            transfer$unknown_file_sizes
          ),
          "."
        )
      )
    })
    output$preview_action <- shiny::renderUI({
      value <- plan()
      if (!is.null(value) && isTRUE(value$preview_ready)) {
        return(shiny::actionButton(
          session$ns("build_preview"),
          "Build preview",
          icon = shiny::icon("table"),
          class = "btn-primary handoff-primary-action"
        ))
      }
      shiny::tags$button(
        type = "button",
        class = "btn btn-primary handoff-primary-action",
        disabled = NA,
        shiny::icon("table"),
        " Build preview"
      )
    })
    output$preview_status <- shiny::renderUI({
      glc_explorer_status_tag(preview_status())
    })
    output$preview_table <- shiny::renderTable(
      {
        glc_explorer_format_preview(preview())
      },
      rownames = FALSE,
      bordered = FALSE,
      spacing = "s"
    )
    output$script_note <- shiny::renderUI({
      value <- plan()
      if (is.null(value)) {
        return(NULL)
      }
      if (isTRUE(value$script_ready)) {
        note <- if (identical(value$mode %||% "data", "metadata")) {
          paste0(
            " The script opens commit ",
            substr(value$commit, 1L, 12L),
            ", downloads only the selected metadata resources, and assigns ",
            "the package handle to local_package and metadata to glc_metadata. ",
            "Temporary handoff objects are removed."
          )
        } else {
          paste0(
            " The script opens commit ",
            substr(value$commit, 1L, 12L),
            ", reuses an existing manifest-backed directory, downloads ",
            "complete included files when needed, and assigns the collected ",
            "result to glc_data. Only glc_data and local_package remain from ",
            "the handoff."
          )
        }
        return(shiny::tags$div(
          class = "alert alert-info py-2",
          role = "note",
          shiny::icon("circle-info"),
          note
        ))
      }
      shiny::tags$div(
        class = "alert alert-warning py-2",
        role = "status",
        "Resolve the selection issues before exporting the R script."
      )
    })
    output$script <- shiny::renderText({
      value <- plan()
      if (is.null(value)) {
        return("# Open a package to generate an R handoff script.")
      }
      if (!isTRUE(value$script_ready)) {
        return(paste(
          c(
            "# R script unavailable:",
            paste0("# - ", value$script_issues)
          ),
          collapse = "\n"
        ))
      }
      glc_explorer_selection_script(value)
    })
    output$script_download_ui <- shiny::renderUI({
      value <- plan()
      if (is.null(value) || !isTRUE(value$script_ready)) {
        return(NULL)
      }
      shiny::downloadButton(
        session$ns("script_download"),
        "Download R script",
        icon = shiny::icon("download"),
        class = "btn-primary handoff-primary-action mb-3"
      )
    })
    output$script_download <- shiny::downloadHandler(
      filename = function() {
        glc_explorer_download_filename(plan())
      },
      content = function(file) {
        value <- plan()
        shiny::req(isTRUE(value$script_ready))
        filename <- glc_explorer_download_filename(value)
        writeLines(
          glc_explorer_selection_script(value),
          con = file,
          useBytes = TRUE
        )
        show_modal(
          glc_explorer_download_complete_modal(
            filename,
            mode = value$mode %||% "data"
          ),
          session = session
        )
      }
    )

    list(
      selection = shiny::reactive(selection()),
      scope = scope,
      group_filters = group_filters,
      group_filter_result = group_filter_result,
      effective_file_group_ids = effective_file_group_ids,
      plan = plan,
      group_summary_page = group_summary_page,
      handoff_mode = handoff_mode,
      applied_preselection = shiny::reactive(applied_preselection()),
      contents_request = shiny::reactive(contents_request()),
      preview = shiny::reactive(preview()),
      status = shiny::reactive(status()),
      preview_status = shiny::reactive(preview_status())
    )
  })
}

selection_handoff_app <- function(package) {
  glc_explorer_check_dependencies()
  if (!inherits(package, "glc_package")) {
    glc_abort("{.arg package} must be opened with {.fn glc_open}.")
  }
  ui <- bslib::page_fluid(
    theme = glc_explorer_theme(),
    selection_handoff_ui("handoff"),
    bslib::card(
      bslib::card_header("Development status"),
      shiny::verbatimTextOutput("module_state")
    )
  )
  server <- function(input, output, session) {
    state <- selection_handoff_server(
      "handoff",
      package = shiny::reactive(package),
      active = shiny::reactive(TRUE)
    )
    output$module_state <- shiny::renderPrint({
      plan <- state$plan()
      list(
        status = state$status(),
        preview_status = state$preview_status(),
        datasets = plan$datasets %||% character(),
        file_groups = plan$file_groups %||% character(),
        variables = plan$variables %||% character(),
        issues = plan$issues %||% character()
      )
    })
  }
  shiny::shinyApp(ui, server)
}
