glc_explorer_package_key <- function(package) {
  if (is.null(package)) {
    return(NULL)
  }
  if (identical(package$source_type, "local")) {
    return(paste("local", package$root, sep = ":"))
  }
  paste("remote", package$repo, package$commit, sep = ":")
}

glc_explorer_load_contents <- function(package) {
  metadata <- glc_metadata(package)
  metadata <- glc_metadata_leaf_table(metadata)
  metadata <- glc_explorer_add_metadata_record_ids(metadata)
  list(
    datasets = glc_datasets(package),
    files = glc_files(package),
    variables = glc_variables(package),
    metadata = metadata
  )
}

glc_explorer_display_values <- function(x) {
  values <- glc_compact_character(x)
  if (length(values) == 0L) {
    return("\u2014")
  }
  paste(values, collapse = ", ")
}

glc_explorer_dataset_table <- function(datasets, variables) {
  variable_count <- integer(nrow(datasets))
  if (nrow(variables) > 0L) {
    counts <- table(variables$dataset_id)
    index <- match(datasets$dataset_id, names(counts))
    matched <- !is.na(index)
    variable_count[matched] <- as.integer(counts[index[matched]])
  }

  data.frame(
    Dataset = datasets$dataset_id,
    Study = datasets$study_id,
    Participant = datasets$participant_id,
    Devices = vapply(
      datasets$device_ids,
      glc_explorer_display_values,
      character(1)
    ),
    Modalities = vapply(
      datasets$modalities,
      glc_explorer_display_values,
      character(1)
    ),
    `File groups` = datasets$file_group_count,
    Files = datasets$file_count,
    Variables = variable_count,
    `Time zone` = datasets$timezone,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

glc_explorer_filter_datasets <- function(datasets, query = "") {
  devices <- vapply(
    datasets$device_ids,
    glc_explorer_display_values,
    character(1)
  )
  modalities <- vapply(
    datasets$modalities,
    glc_explorer_display_values,
    character(1)
  )
  keep <- glc_explorer_matches_query(
    query,
    datasets$dataset_id,
    datasets$study_id,
    datasets$participant_id,
    devices,
    modalities
  )
  datasets[keep, , drop = FALSE]
}

glc_explorer_file_group_inventory <- function(files, variables) {
  if (nrow(files) == 0L) {
    return(data.frame(
      dataset_id = character(),
      file_group_id = character(),
      study_id = character(),
      participant_id = character(),
      role = character(),
      data_state = character(),
      device_id = character(),
      modalities = character(),
      format = character(),
      timezone = character(),
      file_count = integer(),
      available_file_count = integer(),
      variable_count = integer(),
      stringsAsFactors = FALSE
    ))
  }

  keys <- paste(files$dataset_id, files$file_group_id, sep = "\r")
  unique_keys <- unique(keys)
  rows <- lapply(unique_keys, function(key) {
    index <- which(keys == key)
    group_id <- files$file_group_id[[index[[1L]]]]
    variable_count <- sum(variables$file_group_id == group_id)
    data.frame(
      dataset_id = files$dataset_id[[index[[1L]]]],
      file_group_id = group_id,
      study_id = files$study_id[[index[[1L]]]],
      participant_id = files$participant_id[[index[[1L]]]],
      role = files$role[[index[[1L]]]],
      data_state = files$data_state[[index[[1L]]]],
      device_id = files$device_id[[index[[1L]]]],
      modalities = glc_explorer_display_values(
        unlist(files$modalities[index], use.names = FALSE)
      ),
      format = files$format[[index[[1L]]]],
      timezone = files$timezone[[index[[1L]]]],
      file_count = length(index),
      available_file_count = sum(files$available[index] %in% TRUE),
      variable_count = variable_count,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

glc_explorer_filter_file_groups <- function(
  file_groups,
  dataset_ids = "all",
  query = ""
) {
  keep <- rep(TRUE, nrow(file_groups))
  dataset_ids <- unique(as.character(dataset_ids))
  dataset_ids <- dataset_ids[!is.na(dataset_ids) & nzchar(dataset_ids)]
  if (!"all" %in% dataset_ids) {
    keep <- keep & file_groups$dataset_id %in% dataset_ids
  }
  keep <- keep &
    glc_explorer_matches_query(
      query,
      file_groups$dataset_id,
      file_groups$file_group_id,
      file_groups$study_id,
      file_groups$participant_id,
      file_groups$role,
      file_groups$data_state,
      file_groups$device_id,
      file_groups$modalities,
      file_groups$format,
      file_groups$timezone
    )
  file_groups[keep, , drop = FALSE]
}

glc_explorer_file_group_table <- function(file_groups) {
  display <- function(value) {
    value <- as.character(value)
    ifelse(is.na(value) | !nzchar(value), "\u2014", value)
  }
  available <- if (nrow(file_groups) == 0L) {
    character()
  } else {
    paste0(
      file_groups$available_file_count,
      " / ",
      file_groups$file_count
    )
  }
  data.frame(
    Dataset = file_groups$dataset_id,
    `File group` = file_groups$file_group_id,
    Study = display(file_groups$study_id),
    Participant = display(file_groups$participant_id),
    Role = display(file_groups$role),
    State = display(file_groups$data_state),
    Device = display(file_groups$device_id),
    Modalities = display(file_groups$modalities),
    Format = display(file_groups$format),
    Files = file_groups$file_count,
    Available = available,
    Variables = file_groups$variable_count,
    `Time zone` = display(file_groups$timezone),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

glc_explorer_matches_query <- function(query, ...) {
  fields <- list(...)
  if (length(fields) == 0L || length(fields[[1L]]) == 0L) {
    return(logical())
  }
  fields <- lapply(fields, function(value) {
    value <- as.character(value)
    value[is.na(value)] <- ""
    value
  })
  text <- do.call(paste, c(fields, sep = " "))
  query <- trimws(query)
  if (!nzchar(query)) {
    return(rep(TRUE, length(text)))
  }
  grepl(tolower(query), tolower(text), fixed = TRUE)
}

glc_explorer_filter_variables <- function(
  variables,
  dataset_ids = "all",
  query = "",
  primary_only = FALSE
) {
  keep <- rep(TRUE, nrow(variables))
  dataset_ids <- unique(as.character(dataset_ids))
  dataset_ids <- dataset_ids[!is.na(dataset_ids) & nzchar(dataset_ids)]
  if (!"all" %in% dataset_ids) {
    keep <- keep & variables$dataset_id %in% dataset_ids
  }
  if (primary_only) {
    keep <- keep & variables$primary %in% TRUE
  }
  keep <- keep &
    glc_explorer_matches_query(
      query,
      variables$name,
      variables$label,
      variables$unit,
      variables$type,
      variables$term
    )
  variables[keep, , drop = FALSE]
}

glc_explorer_variable_table <- function(variables) {
  data.frame(
    Dataset = variables$dataset_id,
    Group = variables$file_group_id,
    Name = variables$name,
    Label = variables$label,
    Unit = variables$unit,
    Type = variables$type,
    Term = variables$term,
    Primary = ifelse(variables$primary %in% TRUE, "Yes", "No"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

glc_explorer_add_metadata_record_ids <- function(metadata) {
  identifier_fields <- c(
    study = "study_internal_id",
    devices = "device_internal_id",
    participants = "participant_internal_id",
    participant_characteristics = "participant_internal_id",
    datasets = "dataset_internal_id"
  )
  metadata$record_id <- rep(NA_character_, nrow(metadata))
  metadata$datasheet_path <- rep(NA_character_, nrow(metadata))
  if (nrow(metadata) == 0L) {
    return(metadata)
  }

  record <- ifelse(
    is.na(metadata$record),
    "object",
    as.character(metadata$record)
  )
  record_key <- paste(metadata$resource, record, sep = "\r")

  for (resource in names(identifier_fields)) {
    resource_rows <- metadata$resource == resource
    identifier_rows <- resource_rows &
      metadata$field == identifier_fields[[resource]]
    if (!any(identifier_rows)) {
      next
    }

    identifier_keys <- record_key[identifier_rows]
    first_identifier <- !duplicated(identifier_keys)
    identifier_keys <- identifier_keys[first_identifier]
    identifier_values <- metadata$value[identifier_rows][first_identifier]
    matched <- match(record_key[resource_rows], identifier_keys)
    metadata$record_id[resource_rows] <- identifier_values[matched]
  }

  datasheet_rows <- metadata$resource == "device_datasheets"
  metadata$datasheet_path[datasheet_rows] <- vapply(
    metadata$field[datasheet_rows],
    glc_explorer_datasheet_path_from_field,
    character(1)
  )
  field_leaf <- sub("^.*\\.", "", metadata$field)
  datasheet_identifier_fields <- c(
    "device_datasheet_internal_id",
    "datasheet_id"
  )
  datasheet_identifier_rows <- datasheet_rows &
    field_leaf %in% datasheet_identifier_fields
  datasheet_identifier_index <- which(datasheet_identifier_rows)
  if (length(datasheet_identifier_index) > 0L) {
    identifier_field <- metadata$field[datasheet_identifier_index]
    identifier_prefix <- ifelse(
      grepl(".", identifier_field, fixed = TRUE),
      sub("\\.[^.]+$", "", identifier_field),
      ""
    )
    field_priority <- match(
      field_leaf[datasheet_identifier_index],
      datasheet_identifier_fields
    )
    identifier_order <- order(nchar(identifier_prefix), field_priority)

    for (identifier in datasheet_identifier_index[identifier_order]) {
      field <- metadata$field[[identifier]]
      prefix <- if (grepl(".", field, fixed = TRUE)) {
        sub("\\.[^.]+$", "", field)
      } else {
        ""
      }
      same_branch <- datasheet_rows
      if (nzchar(prefix)) {
        same_branch <- same_branch &
          startsWith(metadata$field, paste0(prefix, "."))
      } else {
        same_branch <- same_branch & record_key == record_key[[identifier]]
      }
      metadata$record_id[same_branch] <- metadata$value[[identifier]]
      metadata$datasheet_path[same_branch] <- prefix
    }
  }
  metadata
}

glc_explorer_datasheet_path_from_field <- function(field) {
  extension <- regexpr(
    "\\.(json|yaml|yml)(\\.|$)",
    field,
    ignore.case = TRUE,
    perl = TRUE
  )
  if (extension[[1L]] > 0L) {
    end <- extension[[1L]] + attr(extension, "match.length") - 1L
    return(sub("\\.$", "", substr(field, 1L, end)))
  }
  if (grepl(".", field, fixed = TRUE)) {
    return(sub("\\..*$", "", field))
  }
  "datasheet"
}

glc_explorer_filter_metadata <- function(
  metadata,
  resource = "all",
  query = ""
) {
  keep <- rep(TRUE, nrow(metadata))
  if (!identical(resource, "all")) {
    keep <- keep & metadata$resource == resource
  }
  record_id <- metadata$record_id %||% rep("", nrow(metadata))
  keep <- keep &
    glc_explorer_matches_query(
      query,
      metadata$resource,
      record_id,
      metadata$field,
      metadata$value,
      metadata$context
    )
  metadata[keep, , drop = FALSE]
}

glc_explorer_metadata_table <- function(metadata) {
  record_id <- metadata$record_id %||% rep(NA_character_, nrow(metadata))
  record_id[is.na(record_id) | !nzchar(record_id)] <- "\u2014"
  data.frame(
    Resource = metadata$resource,
    `Record ID` = record_id,
    Context = metadata$context,
    Field = metadata$field,
    Value = metadata$value,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

glc_explorer_metadata_branch_tag <- function(metadata) {
  has_parent <- grepl(".", metadata$field, fixed = TRUE)
  branch <- ifelse(
    has_parent,
    sub("\\..*$", "", metadata$field),
    metadata$field
  )
  leaf <- ifelse(
    has_parent,
    sub("^[^.]+\\.", "", metadata$field),
    "Value"
  )
  leaf <- gsub(".", " \u203a ", leaf, fixed = TRUE)
  context_record_ids <- metadata$record_id %||% character()
  context_record_ids <- unique(
    context_record_ids[!is.na(context_record_ids) & nzchar(context_record_ids)]
  )

  shiny::tagList(lapply(unique(branch), function(name) {
    index <- branch == name
    branch_record_ids <- metadata$record_id[index] %||% character()
    branch_record_ids <- unique(
      branch_record_ids[
        !is.na(branch_record_ids) & nzchar(branch_record_ids)
      ]
    )
    branch_label <- if (
      length(context_record_ids) > 1L && length(branch_record_ids) == 1L
    ) {
      paste(name, branch_record_ids[[1L]], sep = " \u2014 ")
    } else {
      name
    }
    values <- shiny::tagList(lapply(which(index), function(row) {
      shiny::tagList(
        shiny::tags$dt(
          class = "col-sm-5 fw-normal",
          shiny::tags$code(leaf[[row]])
        ),
        shiny::tags$dd(
          class = "col-sm-7 text-break",
          metadata$value[[row]]
        )
      )
    }))
    shiny::tags$details(
      class = "ms-3 mb-2",
      shiny::tags$summary(
        class = "py-1",
        shiny::tags$code(branch_label),
        shiny::tags$span(
          class = "badge text-bg-light ms-2",
          sum(index)
        )
      ),
      shiny::tags$dl(class = "row small mb-2 ms-3", values)
    )
  }))
}

glc_explorer_humanize_metadata_name <- function(name) {
  name <- sub("^datasheet_", "", name)
  name <- gsub("_", " ", name, fixed = TRUE)
  tools::toTitleCase(name)
}

glc_explorer_datasheet_value_tag <- function(metadata, fields) {
  values <- shiny::tagList(lapply(seq_len(nrow(metadata)), function(row) {
    shiny::tagList(
      shiny::tags$dt(
        class = "col-sm-5 fw-normal",
        shiny::tags$code(fields[[row]])
      ),
      shiny::tags$dd(
        class = "col-sm-7 text-break",
        metadata$value[[row]]
      )
    )
  }))
  shiny::tags$dl(class = "row small mb-2 ms-3", values)
}

glc_explorer_datasheet_entry_label <- function(metadata, section, record) {
  leaf <- sub("^.*\\.", "", metadata$relative_field)
  key <- sub(paste0("^", section, "_?"), "", leaf)
  key <- sub("^datasheet_", "", key)
  value <- as.character(metadata$value)
  usable <- !is.na(value) & nzchar(value)
  key <- key[usable]
  value <- value[usable]

  priority_names <- c(
    "id",
    "internal_id",
    "nr",
    "number",
    "name",
    "wavelength",
    "type",
    "model"
  )
  priority <- match(key, priority_names)
  candidates <- which(!is.na(priority))
  if (length(candidates) > 0L) {
    candidates <- candidates[order(priority[candidates])]
    candidates <- candidates[!duplicated(key[candidates])]
  }

  section_name <- glc_explorer_humanize_metadata_name(section)
  entry_name <- switch(
    section,
    datasheet_channel = "Channel",
    datasheet_calibration_spectral_sensitivity = "Calibration point",
    section_name
  )
  index_candidate <- candidates[key[candidates] %in% c("nr", "number")]
  index <- if (length(index_candidate) > 0L) {
    value[[index_candidate[[1L]]]]
  } else {
    as.character(record)
  }

  descriptor_candidates <- candidates[
    !key[candidates] %in% c("nr", "number")
  ]
  descriptor_candidates <- utils::head(descriptor_candidates, 2L)
  if (length(descriptor_candidates) == 0L) {
    return(paste(entry_name, index))
  }
  descriptors <- if (
    identical(section, "datasheet_channel") &&
      "name" %in% key[descriptor_candidates]
  ) {
    name_index <- descriptor_candidates[
      key[descriptor_candidates] == "name"
    ][[1L]]
    value[[name_index]]
  } else {
    paste0(
      key[descriptor_candidates],
      ": ",
      value[descriptor_candidates]
    )
  }
  paste(
    paste(entry_name, index),
    paste(descriptors, collapse = " \u00b7 "),
    sep = " \u2014 "
  )
}

glc_explorer_datasheet_section_tag <- function(metadata, section) {
  section_rows <- metadata$section == section
  rows <- metadata[section_rows, , drop = FALSE]
  records <- unique(rows$record)
  section_label <- switch(
    section,
    datasheet_channel = "Channels",
    datasheet_calibration_spectral_sensitivity = "Calibration Spectral Sensitivity",
    glc_explorer_humanize_metadata_name(section)
  )

  entries <- shiny::tagList(lapply(records, function(record) {
    entry_rows <- rows[rows$record == record, , drop = FALSE]
    fields <- sub("^.*\\.", "", entry_rows$relative_field)
    fields <- sub(paste0("^", section, "_?"), "", fields)
    shiny::tags$details(
      class = "ms-3 mb-2",
      shiny::tags$summary(
        class = "py-1",
        shiny::icon("list"),
        paste0(
          " ",
          glc_explorer_datasheet_entry_label(
            entry_rows,
            section,
            record
          )
        ),
        shiny::tags$span(
          class = "badge text-bg-light ms-2",
          nrow(entry_rows)
        )
      ),
      glc_explorer_datasheet_value_tag(entry_rows, fields)
    )
  }))

  shiny::tags$details(
    class = "ms-3 mb-2",
    open = if (length(records) == 1L) NA else NULL,
    shiny::tags$summary(
      class = "py-1 fw-semibold",
      shiny::icon("layer-group"),
      paste0(" ", section_label),
      shiny::tags$span(
        class = "badge text-bg-light ms-2",
        nrow(rows)
      )
    ),
    entries
  )
}

glc_explorer_datasheet_tag <- function(metadata, open = FALSE) {
  record_ids <- metadata$record_id %||% character()
  record_ids <- unique(record_ids[!is.na(record_ids) & nzchar(record_ids)])
  datasheet_label <- if (length(record_ids) == 0L) {
    "Datasheet"
  } else {
    paste("Datasheet", record_ids[[1L]], sep = " \u2014 ")
  }
  path <- metadata$datasheet_path[[1L]]
  relative_field <- metadata$field
  prefix <- paste0(path, ".")
  in_path <- !is.na(path) & startsWith(relative_field, prefix)
  relative_field[in_path] <- substring(
    relative_field[in_path],
    nchar(prefix) + 1L
  )
  metadata$relative_field <- relative_field

  overview_rows <- is.na(metadata$record)
  overview <- NULL
  if (any(overview_rows)) {
    rows <- metadata[overview_rows, , drop = FALSE]
    overview <- shiny::tags$details(
      class = "ms-3 mb-2",
      open = NA,
      shiny::tags$summary(
        class = "py-1 fw-semibold",
        shiny::icon("circle-info"),
        " Datasheet Details",
        shiny::tags$span(
          class = "badge text-bg-light ms-2",
          nrow(rows)
        )
      ),
      glc_explorer_datasheet_value_tag(rows, rows$relative_field)
    )
  }

  nested <- metadata[!overview_rows, , drop = FALSE]
  sections <- shiny::tagList()
  if (nrow(nested) > 0L) {
    has_parent <- grepl(".", nested$relative_field, fixed = TRUE)
    nested$section <- ifelse(
      has_parent,
      sub("\\..*$", "", nested$relative_field),
      "entries"
    )
    sections <- shiny::tagList(lapply(
      unique(nested$section),
      function(section) {
        glc_explorer_datasheet_section_tag(nested, section)
      }
    ))
  }

  shiny::tags$details(
    class = "ms-3 mb-2",
    open = if (open) NA else NULL,
    shiny::tags$summary(
      class = "py-1",
      shiny::icon("file-lines"),
      paste0(" ", datasheet_label),
      shiny::tags$span(
        class = "badge text-bg-light ms-2",
        nrow(metadata)
      )
    ),
    shiny::tags$p(
      class = "small text-body-secondary ms-3 mb-2",
      "Source: ",
      shiny::tags$code(path)
    ),
    overview,
    sections
  )
}

glc_explorer_device_datasheet_hierarchy_tag <- function(metadata) {
  path <- metadata$datasheet_path %||% rep(NA_character_, nrow(metadata))
  missing_path <- is.na(path) | !nzchar(path)
  path[missing_path] <- vapply(
    metadata$field[missing_path],
    glc_explorer_datasheet_path_from_field,
    character(1)
  )
  metadata$datasheet_path <- path
  paths <- unique(path)
  shiny::tagList(lapply(paths, function(datasheet_path) {
    rows <- metadata[path == datasheet_path, , drop = FALSE]
    glc_explorer_datasheet_tag(rows, open = length(paths) == 1L)
  }))
}

glc_explorer_metadata_context_tag <- function(metadata, open = FALSE) {
  contexts <- unique(metadata$context)
  shiny::tagList(lapply(contexts, function(context) {
    rows <- metadata[metadata$context == context, , drop = FALSE]
    record_ids <- rows$record_id %||% character()
    record_ids <- unique(record_ids[!is.na(record_ids) & nzchar(record_ids)])
    context_label <- if (length(record_ids) == 0L) {
      context
    } else {
      paste(context, paste(record_ids, collapse = ", "), sep = " \u2014 ")
    }
    shiny::tags$details(
      class = "ms-3 mb-2",
      open = if (open) NA else NULL,
      shiny::tags$summary(
        class = "py-1",
        shiny::icon("id-card"),
        paste0(" ", context_label),
        shiny::tags$span(
          class = "badge text-bg-light ms-2",
          nrow(rows)
        )
      ),
      glc_explorer_metadata_branch_tag(rows)
    )
  }))
}

glc_explorer_metadata_hierarchy_tag <- function(metadata, max_values = 500L) {
  metadata <- utils::head(metadata, max_values)
  if (nrow(metadata) == 0L) {
    return(shiny::tags$p(
      class = "text-body-secondary",
      "No metadata values match these filters."
    ))
  }

  resources <- unique(metadata$resource)
  shiny::tagList(lapply(resources, function(resource) {
    rows <- metadata[metadata$resource == resource, , drop = FALSE]
    contexts <- unique(rows$context)
    contents <- if (identical(resource, "device_datasheets")) {
      glc_explorer_device_datasheet_hierarchy_tag(rows)
    } else {
      glc_explorer_metadata_context_tag(
        rows,
        open = length(contexts) == 1L
      )
    }
    shiny::tags$details(
      class = "border rounded px-3 py-2 mb-2",
      open = if (length(resources) == 1L) NA else NULL,
      shiny::tags$summary(
        class = "fw-semibold py-1",
        shiny::icon("folder-tree"),
        paste0(" ", resource),
        shiny::tags$span(
          class = "badge text-bg-secondary ms-2",
          nrow(rows)
        )
      ),
      contents
    )
  }))
}

glc_explorer_contents_status_tag <- function(status) {
  if (identical(status$state, "success")) {
    return(shiny::tags$span(
      class = "visually-hidden",
      role = "status",
      status$message
    ))
  }
  classes <- c(
    empty = "alert alert-secondary py-2",
    loading = "alert alert-info py-2",
    ready = "alert alert-secondary py-2",
    error = "alert alert-danger py-2"
  )
  shiny::tags$div(
    class = unname(classes[[status$state]]),
    role = "status",
    status$message
  )
}

package_contents_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      title = "Filters",
      width = 280,
      shiny::conditionalPanel(
        condition = "input.contents_tab === 'Datasets'",
        shiny::tags$h5("Datasets", class = "mb-3"),
        bslib::tooltip(
          shiny::textInput(
            ns("dataset_query"),
            "Search datasets",
            placeholder = "Search datasets"
          ),
          paste(
            "Search by dataset ID, study ID, participant ID,",
            "device ID, or modality."
          )
        ),
        ns = ns
      ),
      shiny::conditionalPanel(
        condition = "input.contents_tab === 'File groups'",
        shiny::tags$h5("File groups", class = "mb-3"),
        shiny::selectizeInput(
          ns("file_group_dataset_id"),
          "Datasets",
          choices = c("All datasets" = "all"),
          selected = "all",
          multiple = TRUE,
          options = list(placeholder = "Choose datasets")
        ),
        shiny::helpText(
          "Remove 'All datasets' to choose a subset."
        ),
        shiny::tags$div(
          class = "d-flex gap-2 mb-3",
          shiny::actionButton(
            ns("file_group_select_all"),
            "Select all",
            icon = shiny::icon("check-double"),
            class = "btn-sm"
          ),
          shiny::actionButton(
            ns("file_group_clear"),
            "Clear",
            icon = shiny::icon("xmark"),
            class = "btn-sm"
          )
        ),
        shiny::textInput(
          ns("file_group_query"),
          "Search file groups",
          placeholder = "Group, role, modality, device, or format"
        ),
        ns = ns
      ),
      shiny::conditionalPanel(
        condition = "input.contents_tab === 'Variables'",
        shiny::tags$h5("Variables", class = "mb-3"),
        shiny::selectizeInput(
          ns("variable_dataset_id"),
          "Datasets",
          choices = c("All datasets" = "all"),
          selected = "all",
          multiple = TRUE,
          options = list(placeholder = "Choose datasets")
        ),
        shiny::helpText(
          "Remove 'All datasets' to choose a subset."
        ),
        shiny::tags$div(
          class = "d-flex gap-2 mb-3",
          shiny::actionButton(
            ns("variable_select_all"),
            "Select all",
            icon = shiny::icon("check-double"),
            class = "btn-sm"
          ),
          shiny::actionButton(
            ns("variable_clear"),
            "Clear",
            icon = shiny::icon("xmark"),
            class = "btn-sm"
          )
        ),
        shiny::textInput(
          ns("variable_query"),
          "Search variables",
          placeholder = "Name, label, unit, type, or term"
        ),
        shiny::checkboxInput(
          ns("primary_only"),
          "Only primary variables",
          value = FALSE
        ),
        ns = ns
      ),
      shiny::conditionalPanel(
        condition = "input.contents_tab === 'Metadata'",
        shiny::tags$h5("Metadata", class = "mb-3"),
        shiny::tags$div(
          class = "alert alert-info py-2 small",
          role = "note",
          shiny::icon("circle-info"),
          shiny::tags$strong(" Tip: "),
          paste(
            "Use Hierarchy to find a field, search for its field name,",
            "then switch to Table to compare it across all records."
          )
        ),
        shiny::selectInput(
          ns("metadata_resource"),
          "Metadata resource",
          choices = c("All resources" = "all")
        ),
        shiny::textInput(
          ns("metadata_query"),
          "Search metadata",
          placeholder = "Field or value"
        ),
        shiny::radioButtons(
          ns("metadata_view"),
          "View",
          choices = c(
            "Hierarchy" = "hierarchy",
            "Table" = "table"
          ),
          selected = "hierarchy",
          inline = TRUE
        ),
        ns = ns
      )
    ),
    shiny::uiOutput(ns("status_message")),
    bslib::navset_card_tab(
      id = ns("contents_tab"),
      full_screen = TRUE,
      bslib::nav_panel(
        "Datasets",
        shiny::tags$p(
          class = "text-body-secondary",
          shiny::textOutput(ns("dataset_count"), inline = TRUE)
        ),
        shiny::tableOutput(ns("dataset_table"))
      ),
      bslib::nav_panel(
        "File groups",
        shiny::tags$p(
          class = "text-body-secondary",
          shiny::textOutput(ns("file_group_count"), inline = TRUE)
        ),
        shiny::tableOutput(ns("file_group_table"))
      ),
      bslib::nav_panel(
        "Variables",
        shiny::tags$p(
          class = "text-body-secondary",
          shiny::textOutput(ns("variable_count"), inline = TRUE)
        ),
        shiny::tableOutput(ns("variable_table"))
      ),
      bslib::nav_panel(
        "Metadata",
        shiny::tags$p(
          class = "text-body-secondary",
          shiny::textOutput(ns("metadata_count"), inline = TRUE)
        ),
        shiny::conditionalPanel(
          condition = "input.metadata_view === 'hierarchy'",
          shiny::uiOutput(ns("metadata_hierarchy")),
          ns = ns
        ),
        shiny::conditionalPanel(
          condition = "input.metadata_view === 'table'",
          shiny::tableOutput(ns("metadata_table")),
          ns = ns
        )
      )
    )
  )
}

package_contents_server <- function(
  id,
  package,
  active,
  navigation,
  load_contents = glc_explorer_load_contents,
  schedule_after_flush = glc_explorer_after_flush,
  select_nav = bslib::nav_select
) {
  if (!shiny::is.reactive(package)) {
    glc_abort("{.arg package} must be a reactive expression.")
  }
  if (!shiny::is.reactive(active)) {
    glc_abort("{.arg active} must be a reactive expression.")
  }
  if (!shiny::is.reactive(navigation)) {
    glc_abort("{.arg navigation} must be a reactive expression.")
  }

  shiny::moduleServer(id, function(input, output, session) {
    contents <- shiny::reactiveVal(NULL)
    latest_navigation <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(glc_explorer_status(
      "Open a package before exploring its contents.",
      "empty"
    ))
    loaded_key <- NULL
    observed_key <- NULL
    request_id <- 0L

    finish_load <- function(value, key, request) {
      if (!identical(request, request_id)) {
        return()
      }
      result <- tryCatch(load_contents(value), error = identity)
      if (inherits(result, "error")) {
        contents(NULL)
        status(glc_explorer_status(
          paste("Could not load package contents:", conditionMessage(result)),
          "error"
        ))
        return()
      }
      required <- c("datasets", "files", "variables", "metadata")
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
        contents(NULL)
        status(glc_explorer_status(
          "Could not load package contents: the result is incomplete.",
          "error"
        ))
        return()
      }

      contents(result)
      loaded_key <<- key
      file_groups <- glc_explorer_file_group_inventory(
        result$files,
        result$variables
      )
      status(glc_explorer_status(
        sprintf(
          paste0(
            "Loaded %d datasets, %d file groups, %d variables, ",
            "and %d metadata values."
          ),
          nrow(result$datasets),
          nrow(file_groups),
          nrow(result$variables),
          nrow(result$metadata)
        ),
        "success"
      ))
    }

    shiny::observe({
      value <- package()
      is_active <- isTRUE(active())
      key <- glc_explorer_package_key(value)

      if (!identical(key, observed_key)) {
        latest_navigation(NULL)
        observed_key <<- key
      }

      if (is.null(value)) {
        request_id <<- request_id + 1L
        contents(NULL)
        loaded_key <<- NULL
        status(glc_explorer_status(
          "Open a package before exploring its contents.",
          "empty"
        ))
        return()
      }
      if (!is_active) {
        if (!identical(key, loaded_key)) {
          request_id <<- request_id + 1L
          contents(NULL)
          loaded_key <<- NULL
          status(glc_explorer_status(
            "Open Package contents to load datasets and metadata.",
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
      contents(NULL)
      status(glc_explorer_status("Loading package contents\u2026", "loading"))
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
            "Could not start loading package contents:",
            conditionMessage(scheduling_error)
          ),
          "error"
        ))
      }
    })

    apply_navigation <- function(request, value = contents()) {
      if (is.null(request)) {
        return(invisible(NULL))
      }
      tabs <- c("Datasets", "File groups", "Variables", "Metadata")
      valid_request <- is.list(request) &&
        is.character(request$tab) &&
        length(request$tab) == 1L &&
        !is.na(request$tab) &&
        request$tab %in% tabs
      if (!valid_request) {
        glc_abort("The package-contents navigation request is invalid.")
      }

      select_nav(
        id = "contents_tab",
        selected = request$tab,
        session = session
      )
      switch(
        request$tab,
        Datasets = shiny::updateTextInput(
          session,
          "dataset_query",
          value = ""
        ),
        `File groups` = {
          shiny::updateSelectizeInput(
            session,
            "file_group_dataset_id",
            selected = "all"
          )
          shiny::updateTextInput(
            session,
            "file_group_query",
            value = ""
          )
        },
        Variables = {
          shiny::updateSelectizeInput(
            session,
            "variable_dataset_id",
            selected = "all"
          )
          shiny::updateTextInput(
            session,
            "variable_query",
            value = ""
          )
          shiny::updateCheckboxInput(
            session,
            "primary_only",
            value = FALSE
          )
        },
        Metadata = {
          resource <- request$metadata_resource %||% "all"
          if (!is.null(value)) {
            resources <- unique(value$metadata$resource)
            if (!resource %in% c("all", resources)) {
              resource <- "all"
            }
            shiny::updateSelectInput(
              session,
              "metadata_resource",
              selected = resource
            )
          }
          shiny::updateTextInput(
            session,
            "metadata_query",
            value = ""
          )
          shiny::updateRadioButtons(
            session,
            "metadata_view",
            selected = "hierarchy"
          )
        }
      )
      invisible(NULL)
    }

    shiny::observeEvent(
      navigation(),
      {
        request <- navigation()
        latest_navigation(request)
        apply_navigation(request)
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    shiny::observeEvent(
      contents(),
      {
        value <- contents()
        if (is.null(value)) {
          return()
        }
        dataset_ids <- unique(value$datasets$dataset_id)
        dataset_choices <- c(
          "All datasets" = "all",
          stats::setNames(dataset_ids, dataset_ids)
        )
        shiny::updateSelectizeInput(
          session,
          "file_group_dataset_id",
          choices = dataset_choices,
          selected = "all"
        )
        shiny::updateSelectizeInput(
          session,
          "variable_dataset_id",
          choices = dataset_choices,
          selected = "all"
        )
        resources <- sort(unique(value$metadata$resource))
        request <- latest_navigation()
        requested_resource <- request$metadata_resource %||% "all"
        selected_resource <- if (requested_resource %in% resources) {
          requested_resource
        } else {
          "all"
        }
        shiny::updateSelectInput(
          session,
          "metadata_resource",
          choices = c("All resources" = "all", resources),
          selected = selected_resource
        )
        apply_navigation(request, value)
      },
      ignoreNULL = TRUE
    )

    shiny::observeEvent(
      input$file_group_select_all,
      {
        shiny::updateSelectizeInput(
          session,
          "file_group_dataset_id",
          selected = "all"
        )
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$file_group_clear,
      {
        shiny::updateSelectizeInput(
          session,
          "file_group_dataset_id",
          selected = character()
        )
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$variable_select_all,
      {
        shiny::updateSelectizeInput(
          session,
          "variable_dataset_id",
          selected = "all"
        )
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$variable_clear,
      {
        shiny::updateSelectizeInput(
          session,
          "variable_dataset_id",
          selected = character()
        )
      },
      ignoreInit = TRUE
    )

    filtered_datasets <- shiny::reactive({
      value <- contents()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_filter_datasets(
        value$datasets,
        query = input$dataset_query %||% ""
      )
    })

    file_groups <- shiny::reactive({
      value <- contents()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_file_group_inventory(value$files, value$variables)
    })

    filtered_file_groups <- shiny::reactive({
      value <- file_groups()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_filter_file_groups(
        value,
        dataset_ids = input$file_group_dataset_id %||% character(),
        query = input$file_group_query %||% ""
      )
    })

    filtered_variables <- shiny::reactive({
      value <- contents()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_filter_variables(
        value$variables,
        dataset_ids = input$variable_dataset_id %||% character(),
        query = input$variable_query %||% "",
        primary_only = input$primary_only %||% FALSE
      )
    })

    filtered_metadata <- shiny::reactive({
      value <- contents()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_filter_metadata(
        value$metadata,
        resource = input$metadata_resource %||% "all",
        query = input$metadata_query %||% ""
      )
    })

    output$status_message <- shiny::renderUI({
      glc_explorer_contents_status_tag(status())
    })
    output$dataset_count <- shiny::renderText({
      value <- filtered_datasets()
      if (is.null(value)) {
        return("No datasets loaded.")
      }
      sprintf("%d dataset(s) match the filters.", nrow(value))
    })
    output$file_group_count <- shiny::renderText({
      value <- filtered_file_groups()
      if (is.null(value)) {
        return("No file groups loaded.")
      }
      sprintf("%d file group(s) match the filters.", nrow(value))
    })
    output$variable_count <- shiny::renderText({
      value <- filtered_variables()
      if (is.null(value)) {
        return("No variables loaded.")
      }
      sprintf("%d variable declaration(s) match the filters.", nrow(value))
    })
    output$metadata_count <- shiny::renderText({
      value <- filtered_metadata()
      if (is.null(value)) {
        return("No metadata loaded.")
      }
      hierarchy <- identical(input$metadata_view %||% "hierarchy", "hierarchy")
      limit <- if (hierarchy) 500L else 200L
      shown <- min(limit, nrow(value))
      sprintf(
        "Showing %d of %d matching metadata value(s).",
        shown,
        nrow(value)
      )
    })
    output$dataset_table <- shiny::renderTable(
      {
        value <- filtered_datasets()
        all_contents <- contents()
        if (is.null(value) || is.null(all_contents)) {
          return(NULL)
        }
        glc_explorer_dataset_table(value, all_contents$variables)
      },
      rownames = FALSE,
      bordered = FALSE,
      spacing = "m"
    )
    output$file_group_table <- shiny::renderTable(
      {
        value <- filtered_file_groups()
        if (is.null(value)) {
          return(NULL)
        }
        glc_explorer_file_group_table(value)
      },
      rownames = FALSE,
      bordered = FALSE,
      spacing = "m"
    )
    output$variable_table <- shiny::renderTable(
      {
        value <- filtered_variables()
        if (is.null(value)) {
          return(NULL)
        }
        glc_explorer_variable_table(value)
      },
      rownames = FALSE,
      bordered = FALSE,
      spacing = "m"
    )
    output$metadata_table <- shiny::renderTable(
      {
        value <- filtered_metadata()
        if (is.null(value)) {
          return(NULL)
        }
        glc_explorer_metadata_table(utils::head(value, 200L))
      },
      rownames = FALSE,
      bordered = FALSE,
      spacing = "m"
    )
    output$metadata_hierarchy <- shiny::renderUI({
      value <- filtered_metadata()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_metadata_hierarchy_tag(value)
    })

    list(
      contents = shiny::reactive(contents()),
      datasets = filtered_datasets,
      file_groups = filtered_file_groups,
      variables = filtered_variables,
      metadata = filtered_metadata,
      status = shiny::reactive(status())
    )
  })
}

package_contents_app <- function(package) {
  glc_explorer_check_dependencies()
  if (!inherits(package, "glc_package")) {
    glc_abort("{.arg package} must be opened with {.fn glc_open}.")
  }
  ui <- bslib::page_fluid(
    theme = glc_explorer_theme(),
    package_contents_ui("contents"),
    bslib::card(
      bslib::card_header("Development status"),
      shiny::verbatimTextOutput("module_state")
    )
  )
  server <- function(input, output, session) {
    current_package <- shiny::reactive(package)
    active <- shiny::reactive(TRUE)
    navigation <- shiny::reactive(NULL)
    state <- package_contents_server(
      "contents",
      current_package,
      active,
      navigation
    )
    output$module_state <- shiny::renderPrint({
      list(
        status = state$status(),
        dataset_rows = nrow(state$datasets() %||% data.frame()),
        file_group_rows = nrow(state$file_groups() %||% data.frame()),
        variable_rows = nrow(state$variables() %||% data.frame()),
        metadata_rows = nrow(state$metadata() %||% data.frame())
      )
    })
  }
  shiny::shinyApp(ui, server)
}
