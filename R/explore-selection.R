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
        type = vapply(
          group$variables,
          function(variable) variable$type,
          character(1)
        ),
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
        format = group$format,
        timezone = group$timezone,
        modalities = list(group$modality),
        role = group$role,
        data_state = group$data_state,
        datetime_source = group$datetime$source,
        datetime_date = group$datetime$date,
        datetime_format = group$datetime$date_format,
        datetime_time = group$datetime$time,
        datetime_time_format = group$datetime$time_format,
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
    format = character(),
    timezone = character(),
    modalities = list(),
    role = character(),
    data_state = character(),
    datetime_source = character(),
    datetime_date = character(),
    datetime_format = character(),
    datetime_time = character(),
    datetime_time_format = character(),
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

glc_explorer_age_slider_spec <- function(value) {
  ages <- suppressWarnings(as.numeric(value %||% numeric()))
  ages <- ages[is.finite(ages)]
  if (length(ages) == 0L) {
    return(NULL)
  }
  bounds <- range(ages)
  whole_years <- all(abs(ages - round(ages)) < sqrt(.Machine$double.eps))
  step <- if (whole_years) 1 else NULL
  if (identical(bounds[[1L]], bounds[[2L]])) {
    bounds[[2L]] <- bounds[[2L]] + if (whole_years) 1 else 0.1
  }
  list(
    min = bounds[[1L]],
    max = bounds[[2L]],
    value = bounds,
    step = step
  )
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
    matched <- characteristics$participant_id[
      characteristics$characteristic_name %in%
        characteristic_name &
        characteristics$characteristic_value %in% characteristic_values
    ]
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

glc_explorer_datetime_signature <- function(groups) {
  paste(
    groups$datetime_source,
    groups$datetime_date,
    groups$datetime_format,
    groups$datetime_time,
    groups$datetime_time_format,
    sep = "|"
  )
}

glc_explorer_selection_compatibility <- function(
  groups,
  variable_names
) {
  issues <- character()
  variable_names <- glc_explorer_nonempty_values(variable_names)
  if (nrow(groups) == 0L) {
    return(list(ok = FALSE, issues = "No file groups are included."))
  }
  if (length(variable_names) == 0L) {
    return(list(
      ok = FALSE,
      issues = "No source variables are available for the included file groups."
    ))
  }
  unsupported <- unique(groups$format[
    !groups$format %in% c("csv", "txt", "tsv")
  ])
  unsupported <- glc_explorer_nonempty_values(unsupported)
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

  columns <- vector("list", nrow(groups))
  types <- vector("list", nrow(groups))
  for (index in seq_len(nrow(groups))) {
    variables <- groups$variables[[index]]
    selected <- variables$name %in% variable_names
    columns[[index]] <- variables$name[selected]
    types[[index]] <- paste(
      variables$name[selected],
      variables$type[selected],
      sep = "="
    )
    if (length(columns[[index]]) == 0L) {
      issues <- c(
        issues,
        paste0(
          "File group ",
          groups$file_group_id[[index]],
          " does not provide any of the chosen source variables."
        )
      )
    }
  }
  if (length(columns) > 1L) {
    same_columns <- vapply(
      columns[-1L],
      identical,
      logical(1),
      columns[[1L]]
    )
    same_types <- vapply(
      types[-1L],
      identical,
      logical(1),
      types[[1L]]
    )
    if (!all(same_columns)) {
      issues <- c(issues, "Included file groups use different source columns.")
    } else if (!all(same_types)) {
      issues <- c(
        issues,
        "Included file groups use different source variable types."
      )
    }
  }

  compare_values <- function(values, label) {
    if (length(unique(values)) > 1L) {
      issues <<- c(
        issues,
        paste0("Included file groups use different ", label, ".")
      )
    }
  }
  if (nrow(groups) > 0L) {
    compare_values(as.character(groups$timezone), "time zones")
    compare_values(
      vapply(groups$modalities, paste, character(1), collapse = "|"),
      "modalities"
    )
    compare_values(as.character(groups$role), "file roles")
    compare_values(as.character(groups$data_state), "data states")
    compare_values(
      glc_explorer_datetime_signature(groups),
      "datetime specifications"
    )

    invalid_timezone <- is.na(groups$timezone) |
      !groups$timezone %in% OlsonNames()
    if (any(invalid_timezone)) {
      issues <- c(issues, "Included file groups contain invalid time zones.")
    }
    incomplete_datetime <- is.na(groups$datetime_date) |
      !nzchar(groups$datetime_date) |
      is.na(groups$datetime_format) |
      !nzchar(groups$datetime_format) |
      (!is.na(groups$datetime_time) &
        nzchar(groups$datetime_time) &
        (is.na(groups$datetime_time_format) |
          !nzchar(groups$datetime_time_format)))
    if (any(incomplete_datetime)) {
      issues <- c(
        issues,
        "Included file groups contain incomplete datetime specifications."
      )
    }
  }
  issues <- unique(issues)
  list(ok = length(issues) == 0L, issues = issues)
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

glc_explorer_selection_hash <- function(package_info, datasets, groups) {
  value <- list(
    repository = package_info$repository,
    commit = package_info$commit,
    dataset_ids = sort(unique(datasets)),
    file_group_ids = sort(unique(groups))
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
  participant_ids = character(),
  device_ids = character(),
  dataset_ids = character(),
  file_group_ids = character(),
  variables = character(),
  standardize = c("lightlogr", "none")
) {
  standardize <- match.arg(standardize)
  scope <- glc_explorer_selection_scope(
    selection,
    facets,
    participant_ids = participant_ids,
    device_ids = device_ids
  )
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
  requested_groups <- glc_explorer_nonempty_values(file_group_ids)
  groups <- available_groups
  if (length(requested_groups) > 0L) {
    groups <- groups[
      groups$file_group_id %in% requested_groups,
      ,
      drop = FALSE
    ]
  }
  resolved_dataset_ids <- unique(groups$dataset_id)
  group_ids <- unique(groups$file_group_id)
  available_variables <- selection$variables[
    selection$variables$file_group_id %in% group_ids,
    ,
    drop = FALSE
  ]
  requested_variables <- glc_explorer_nonempty_values(variables)
  variable_filter_active <- length(requested_variables) > 0L
  selected_variables <- if (variable_filter_active) {
    unique(available_variables$name[
      available_variables$name %in% requested_variables
    ])
  } else {
    unique(available_variables$name)
  }
  variable_filter <- if (variable_filter_active) {
    selected_variables
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
  if (length(selected_datasets) > 0L && nrow(groups) == 0L) {
    issues <- c(
      issues,
      if (length(requested_groups) > 0L) {
        "None of the chosen file groups is available for the current filters."
      } else {
        "No file groups are available for the selected datasets."
      }
    )
  }
  compatibility <- if (nrow(groups) > 0L) {
    glc_explorer_selection_compatibility(groups, selected_variables)
  } else {
    list(ok = FALSE, issues = character())
  }
  issues <- unique(c(issues, compatibility$issues))
  package_info <- glc_explorer_package_selection_info(package)
  script_issues <- unique(c(
    issues,
    glc_explorer_script_source_issues(package_info)
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
    group_ids
  )
  directory_name <- paste(
    glc_explorer_safe_path_component(package_info$package_id),
    substr(package_info$commit, 1L, 12L),
    selection_hash,
    sep = "-"
  )

  list(
    package_id = package_info$package_id,
    repository = package_info$repository,
    commit = package_info$commit,
    registry_generated_at = package_info$registry_generated_at,
    facets = facets,
    requested = list(
      participant_ids = glc_explorer_nonempty_values(participant_ids),
      device_ids = glc_explorer_nonempty_values(device_ids),
      dataset_ids = requested_datasets,
      file_group_ids = requested_groups,
      variables = requested_variables
    ),
    participants = resolved_participants,
    devices = resolved_devices,
    datasets = resolved_dataset_ids,
    file_groups = group_ids,
    variables = selected_variables,
    variable_filter = variable_filter,
    variable_filter_active = variable_filter_active,
    files = files,
    estimated_bytes = estimated_bytes,
    unknown_file_sizes = sum(!known_bytes),
    preview_files = preview_files,
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
    "  variables = source_variables",
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

glc_explorer_preview_selection <- function(package, plan, n_max = 10L) {
  if (!isTRUE(plan$preview_ready)) {
    glc_abort(
      paste(
        "Cannot build the preview:",
        paste(plan$issues, collapse = " ")
      ),
      class = "glcdp_explorer_invalid_selection"
    )
  }
  collection <- glc_read(
    package,
    dataset_id = plan$datasets,
    file_group = plan$file_groups,
    files = plan$preview_files,
    variables = plan$variable_filter,
    n_max = glc_explorer_preview_row_limit(n_max),
    progress = FALSE
  )
  result <- glc_collect(collection, standardize = plan$standardization)
  tibble::as_tibble(result)
}

glc_explorer_download_filename <- function(plan) {
  paste0(
    glc_explorer_safe_path_component(plan$package_id),
    "-selection.R"
  )
}

glc_explorer_download_complete_modal <- function(filename) {
  shiny::modalDialog(
    title = shiny::tagList(
      shiny::icon("circle-check"),
      "R script downloaded"
    ),
    shiny::tags$p(
      "Use ",
      shiny::tags$code(filename),
      paste0(
        " in R to download, import, and collect the data exactly as ",
        "specified in your selection."
      )
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

glc_explorer_default_variables <- function(variables) {
  primary <- unique(variables$name[variables$primary %in% TRUE])
  if (length(primary) > 0L) primary else unique(variables$name)
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
  file_group_ids = character()
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
  file_group_ids = character()
) {
  groups <- glc_explorer_resolved_group_rows(
    selection,
    scope,
    dataset_ids,
    file_group_ids
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

glc_explorer_selection_summary_table <- function(plan) {
  variable_selection <- if (isTRUE(plan$variable_filter_active)) {
    glc_explorer_display_values(plan$variables)
  } else {
    paste0("All available (", length(plan$variables), ")")
  }
  data.frame(
    Selection = c(
      "Participants",
      "Devices",
      "Datasets",
      "File groups",
      "Source variables",
      "Files",
      "Preview files",
      "Estimated transfer",
      "Collection mode",
      "Relative data directory"
    ),
    Value = c(
      glc_explorer_display_values(plan$participants),
      glc_explorer_display_values(plan$devices),
      glc_explorer_display_values(plan$datasets),
      glc_explorer_display_values(plan$file_groups),
      variable_selection,
      nrow(plan$files),
      length(plan$preview_files),
      glc_explorer_format_bytes(
        plan$estimated_bytes,
        plan$unknown_file_sizes
      ),
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
  data.frame(
    Dataset = groups$dataset_id,
    `File group` = groups$file_group_id,
    Device = ifelse(
      is.na(groups$device_id) | !nzchar(groups$device_id),
      "\u2014",
      groups$device_id
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
  if (isTRUE(plan$script_ready)) {
    variables <- if (isTRUE(plan$variable_filter_active)) {
      paste0(length(plan$variables), " source variable(s)")
    } else {
      paste0("all ", length(plan$variables), " available source variable(s)")
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
      )
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

selection_handoff_ui <- function(id) {
  ns <- shiny::NS(id)
  multi_select <- function(input_id, label, placeholder) {
    shiny::selectizeInput(
      ns(input_id),
      label,
      choices = character(),
      selected = character(),
      multiple = TRUE,
      options = list(placeholder = placeholder)
    )
  }

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      title = "Build a selection",
      width = 330,
      bslib::accordion(
        open = "Data selection",
        multiple = TRUE,
        bslib::accordion_panel(
          "Participants",
          shiny::uiOutput(ns("participant_age_filter")),
          multi_select("participant_sex", "Sex", "Any sex"),
          multi_select("participant_gender", "Gender", "Any gender"),
          shiny::selectizeInput(
            ns("characteristic_name"),
            "Characteristic",
            choices = c("No characteristic filter" = ""),
            selected = "",
            multiple = FALSE
          ),
          multi_select(
            "characteristic_values",
            "Characteristic values",
            "Any value"
          ),
          multi_select(
            "participant_ids",
            "Participant IDs",
            "All matching participants"
          ),
          shiny::helpText(
            "Choices within a filter use OR; active filters combine with AND."
          )
        ),
        bslib::accordion_panel(
          "Devices",
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
        ),
        bslib::accordion_panel(
          "Data selection",
          multi_select(
            "dataset_ids",
            "Datasets (required)",
            "Choose one or more datasets"
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
          multi_select(
            "file_group_ids",
            "File groups (optional)",
            "All file groups"
          ),
          shiny::helpText(
            paste0(
              "Leave empty to include every file group in the selected ",
              "datasets. Choose a subset when groups cannot be collected ",
              "together."
            )
          ),
          shiny::actionButton(
            ns("file_groups_use_all"),
            "Use all file groups",
            icon = shiny::icon("layer-group"),
            class = "btn-sm mb-3"
          ),
          multi_select(
            "variables",
            "Source variables (optional)",
            "All source variables"
          ),
          shiny::helpText(
            "Leave empty to import every variable from the included groups."
          ),
          shiny::tags$div(
            class = "d-flex flex-wrap gap-2 mb-3",
            shiny::actionButton(
              ns("variables_recommended"),
              "Recommended",
              icon = shiny::icon("star"),
              class = "btn-sm"
            ),
            shiny::actionButton(
              ns("variables_select_all"),
              "Select all",
              icon = shiny::icon("check-double"),
              class = "btn-sm"
            ),
            shiny::actionButton(
              ns("variables_clear"),
              "Use all variables",
              icon = shiny::icon("asterisk"),
              class = "btn-sm"
            )
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
      )
    ),
    shiny::uiOutput(ns("status_message")),
    shiny::uiOutput(ns("plan_status")),
    bslib::navset_card_tab(
      id = ns("handoff_tab"),
      full_screen = TRUE,
      bslib::nav_panel(
        "Selection summary",
        shiny::tableOutput(ns("selection_summary")),
        shiny::tags$h5("Included file groups", class = "mt-3"),
        shiny::tableOutput(ns("group_summary")),
        shiny::uiOutput(ns("metadata_notice")),
        shiny::tags$div(
          class = "d-flex justify-content-end mt-3",
          shiny::actionButton(
            ns("summary_continue"),
            "Continue to preview",
            icon = shiny::icon("arrow-right"),
            class = "btn-primary"
          )
        ),
        value = "summary",
        icon = shiny::icon("1")
      ),
      bslib::nav_panel(
        "Preview",
        shiny::uiOutput(ns("transfer_note")),
        shiny::tags$div(
          class = "d-flex align-items-end flex-wrap gap-3",
          shiny::numericInput(
            ns("preview_rows"),
            "Rows to read per file",
            value = 10L,
            min = 1L,
            max = 1000L,
            step = 1L,
            width = "12rem"
          ),
          shiny::uiOutput(ns("preview_action"))
        ),
        shiny::uiOutput(ns("preview_status")),
        shiny::tableOutput(ns("preview_table")),
        shiny::tags$div(
          class = "d-flex justify-content-end mt-3",
          shiny::actionButton(
            ns("preview_continue"),
            "Continue to Export to R",
            icon = shiny::icon("arrow-right"),
            class = "btn-primary"
          )
        ),
        value = "preview",
        icon = shiny::icon("2")
      ),
      bslib::nav_panel(
        "Export to R (R script)",
        shiny::uiOutput(ns("script_note")),
        shiny::uiOutput(ns("script_download_ui")),
        shiny::verbatimTextOutput(ns("script"), placeholder = TRUE),
        value = "export",
        icon = shiny::icon("3")
      )
    )
  )
}

selection_handoff_server <- function(
  id,
  package,
  active,
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

  shiny::moduleServer(id, function(input, output, session) {
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
    request_id <- 0L
    preview_request_id <- 0L

    age_slider_spec <- shiny::reactive({
      value <- selection()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_age_slider_spec(value$participants$age)
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

    update_choices <- function(id, choices, selected = character()) {
      shiny::updateSelectizeInput(
        session,
        id,
        choices = choices,
        selected = intersect(
          glc_explorer_nonempty_values(selected),
          unname(choices)
        )
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
        "characteristic_values",
        "participant_ids",
        "device_manufacturer",
        "device_model",
        "device_sensor_type",
        "device_ids",
        "dataset_ids",
        "file_group_ids",
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
    }

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
    }

    shiny::observe({
      value <- package()
      key <- glc_explorer_package_key(value)
      if (is.null(value)) {
        request_id <<- request_id + 1L
        selection(NULL)
        loaded_key <<- NULL
        clear_inputs()
        status(glc_explorer_status(
          "Open a package before building a selection.",
          "empty"
        ))
        return()
      }
      if (!isTRUE(active())) {
        if (!identical(key, loaded_key)) {
          request_id <<- request_id + 1L
          selection(NULL)
          loaded_key <<- NULL
          clear_inputs()
          status(glc_explorer_status(
            "Open Select & hand off to load selection metadata.",
            "ready"
          ))
        }
        return()
      }
      if (identical(key, loaded_key)) {
        return()
      }

      request_id <<- request_id + 1L
      request <- request_id
      selection(NULL)
      clear_inputs()
      status(glc_explorer_status("Loading selection metadata\u2026", "loading"))
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

    shiny::observeEvent(
      input$characteristic_name,
      {
        value <- selection()
        if (is.null(value)) {
          return()
        }
        name <- glc_explorer_nonempty_values(input$characteristic_name)
        values <- if (length(name) == 0L) {
          character()
        } else {
          value$participant_characteristics$characteristic_value[
            value$participant_characteristics$characteristic_name %in% name
          ]
        }
        update_multi(
          "characteristic_values",
          values,
          shiny::isolate(input$characteristic_values)
        )
      },
      ignoreInit = TRUE
    )

    facets <- shiny::reactive({
      age_spec <- age_slider_spec()
      list(
        participant = list(
          age = glc_explorer_age_filter_value(
            input$participant_age,
            if (is.null(age_spec)) numeric() else age_spec$value
          ),
          sex = input$participant_sex %||% character(),
          gender = input$participant_gender %||% character(),
          characteristic_name = input$characteristic_name %||% character(),
          characteristic_values = input$characteristic_values %||% character()
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
        update_multi(
          "participant_ids",
          eligible_participants(),
          shiny::isolate(input$participant_ids)
        )
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      eligible_devices(),
      {
        update_multi(
          "device_ids",
          eligible_devices(),
          shiny::isolate(input$device_ids)
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

    shiny::observeEvent(
      scope(),
      {
        value <- scope()
        if (is.null(value)) {
          return()
        }
        update_multi(
          "dataset_ids",
          value$dataset_ids,
          shiny::isolate(input$dataset_ids)
        )
      },
      ignoreInit = TRUE
    )

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

    shiny::observeEvent(
      available_groups(),
      {
        value <- available_groups()
        if (is.null(value)) {
          return()
        }
        update_choices(
          "file_group_ids",
          glc_explorer_file_group_choices(value),
          shiny::isolate(input$file_group_ids)
        )
      },
      ignoreInit = TRUE
    )

    available_variables <- shiny::reactive({
      value <- selection()
      current_scope <- scope()
      if (is.null(value) || is.null(current_scope)) {
        return(NULL)
      }
      glc_explorer_available_variables(
        value,
        current_scope,
        input$dataset_ids %||% character(),
        input$file_group_ids %||% character()
      )
    })

    shiny::observeEvent(
      available_variables(),
      {
        value <- available_variables()
        if (is.null(value)) {
          return()
        }
        choices <- unique(value$name)
        selected <- intersect(
          glc_explorer_nonempty_values(shiny::isolate(input$variables)),
          choices
        )
        update_multi("variables", choices, selected)
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
      input$variables_recommended,
      {
        value <- available_variables()
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
      input$variables_select_all,
      {
        value <- available_variables()
        if (!is.null(value)) {
          update_multi("variables", unique(value$name), unique(value$name))
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$variables_clear,
      {
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
        select_nav("handoff_tab", selected = "preview", session = session)
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
      value <- selection()
      package_value <- package()
      if (is.null(value) || is.null(package_value)) {
        return(NULL)
      }
      glc_explorer_build_selection_plan(
        package_value,
        value,
        facets(),
        participant_ids = input$participant_ids %||% character(),
        device_ids = input$device_ids %||% character(),
        dataset_ids = input$dataset_ids %||% character(),
        file_group_ids = input$file_group_ids %||% character(),
        variables = input$variables %||% character(),
        standardize = input$standardization %||% "lightlogr"
      )
    })

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
        preview_status(glc_explorer_status(
          "Reading the first declared file from each included group\u2026",
          "loading"
        ))
        row_limit <- glc_explorer_preview_row_limit(input$preview_rows)
        scheduling_error <- tryCatch(
          {
            schedule_after_flush(
              function() {
                result <- tryCatch(
                  preview_selection(package_value, value, row_limit),
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
                      "limit of %d row(s) per file."
                    ),
                    nrow(result),
                    row_limit
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
    output$group_summary <- shiny::renderTable(
      {
        value <- selection()
        plan_value <- plan()
        if (is.null(value) || is.null(plan_value)) {
          return(NULL)
        }
        glc_explorer_selection_group_table(value, plan_value)
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
      shiny::tags$div(
        class = "alert alert-info py-2",
        role = "note",
        shiny::icon("circle-info"),
        paste0(
          " The preview reads at most ",
          glc_explorer_preview_row_limit(input$preview_rows),
          " rows from the first declared file in each included group. ",
          "Remote files must still be materialized completely. Estimated ",
          "whole-file transfer for this selection: ",
          glc_explorer_format_bytes(
            value$estimated_bytes,
            value$unknown_file_sizes
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
          class = "btn-primary mb-3"
        ))
      }
      shiny::tags$button(
        type = "button",
        class = "btn btn-primary mb-3",
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
        preview()
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
        return(shiny::tags$div(
          class = "alert alert-info py-2",
          role = "note",
          shiny::icon("circle-info"),
          paste0(
            " The script opens commit ",
            substr(value$commit, 1L, 12L),
            ", reuses an existing manifest-backed directory, downloads ",
            "complete included files when needed, and assigns the collected ",
            "result to glc_data."
          )
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
        return("# Open a package and choose data to generate an R script.")
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
        class = "btn-primary mb-3"
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
          glc_explorer_download_complete_modal(filename),
          session = session
        )
      }
    )

    list(
      selection = shiny::reactive(selection()),
      scope = scope,
      plan = plan,
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
