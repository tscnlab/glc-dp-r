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
  metadata <- glc_explorer_metadata_leaf_table(metadata)
  metadata <- glc_explorer_add_metadata_record_ids(metadata)
  list(
    datasets = glc_datasets(package),
    files = glc_files(package),
    variables = glc_variables(package),
    metadata = metadata
  )
}

glc_explorer_metadata_leaf_table <- function(metadata) {
  rows <- list(
    resource = list(),
    record = list(),
    field = list(),
    value = list(),
    context = list(),
    .record_path = list(),
    .record_collection = list(),
    .record_index = list(),
    .value_index = list(),
    .structural_path = list()
  )
  row_index <- 0L

  append_field <- function(parent, child) {
    if (nzchar(parent)) paste(parent, child, sep = ".") else child
  }
  append_record <- function(path, collection, index) {
    segment <- paste0(collection, "[", index, "]")
    if (nzchar(path)) paste(path, segment, sep = "\r") else segment
  }
  context_label <- function(record, record_path) {
    root <- if (is.na(record)) "object" else paste("record", record)
    segments <- strsplit(record_path, "\r", fixed = TRUE)[[1L]]
    nested <- segments[!startsWith(segments, ".root[")]
    if (length(nested) == 0L) {
      return(root)
    }
    nested <- sub("\\[([0-9]+)\\]$", " \\1", nested)
    nested <- vapply(
      nested,
      function(segment) {
        glc_explorer_humanize_metadata_name(sub("^.*\\.", "", segment))
      },
      character(1)
    )
    paste(c(root, nested), collapse = " \u203a ")
  }
  add_leaf <- function(
    resource,
    record,
    field,
    value,
    record_path,
    record_collection = NA_character_,
    record_index = NA_integer_,
    force_array = FALSE
  ) {
    if (length(value) == 0L) {
      return(invisible(NULL))
    }
    values <- as.character(value)
    value_indices <- if (force_array || length(values) > 1L) {
      seq_along(values)
    } else {
      NA_integer_
    }
    for (item in seq_along(values)) {
      row_index <<- row_index + 1L
      value_index <- value_indices[[item]]
      leaf_path <- field
      if (!is.na(value_index)) {
        leaf_path <- paste0(leaf_path, "[", value_index, "]")
      }
      rows$resource[[row_index]] <<- resource
      rows$record[[row_index]] <<- as.integer(record)
      rows$field[[row_index]] <<- field
      rows$value[[row_index]] <<- values[[item]]
      rows$context[[row_index]] <<- context_label(record, record_path)
      rows$.record_path[[row_index]] <<- record_path
      rows$.record_collection[[row_index]] <<- record_collection
      rows$.record_index[[row_index]] <<- as.integer(record_index)
      rows$.value_index[[row_index]] <<- as.integer(value_index)
      rows$.structural_path[[row_index]] <<- paste(
        record_path,
        leaf_path,
        sep = "\r"
      )
    }
    invisible(NULL)
  }

  walk_value <- NULL
  walk_named_object <- NULL
  walk_frame_row <- NULL
  walk_record_frame <- NULL

  walk_named_object <- function(
    value,
    resource,
    field,
    record,
    record_path,
    record_collection = NA_character_,
    record_index = NA_integer_
  ) {
    child_names <- names(value)
    for (i in seq_along(value)) {
      name <- child_names[[i]]
      child_field <- append_field(field, name)
      walk_value(
        value[[i]],
        resource = resource,
        field = child_field,
        record = record,
        record_path = record_path,
        record_collection = record_collection,
        record_index = record_index,
        from_list_column = FALSE
      )
    }
    invisible(NULL)
  }

  walk_frame_row <- function(
    value,
    row,
    resource,
    field,
    record,
    record_path,
    record_collection = NA_character_,
    record_index = NA_integer_
  ) {
    for (name in names(value)) {
      column <- value[[name]]
      child_field <- append_field(field, name)
      if (is.data.frame(column)) {
        walk_frame_row(
          column,
          row = row,
          resource = resource,
          field = child_field,
          record = record,
          record_path = record_path,
          record_collection = record_collection,
          record_index = record_index
        )
      } else if (is.list(column)) {
        walk_value(
          column[[row]],
          resource = resource,
          field = child_field,
          record = record,
          record_path = record_path,
          record_collection = record_collection,
          record_index = record_index,
          from_list_column = TRUE
        )
      } else {
        add_leaf(
          resource,
          record,
          child_field,
          column[[row]],
          record_path,
          record_collection,
          record_index
        )
      }
    }
    invisible(NULL)
  }

  walk_record_frame <- function(
    value,
    resource,
    field,
    record,
    record_path
  ) {
    if (nrow(value) == 0L) {
      return(invisible(NULL))
    }
    for (row in seq_len(nrow(value))) {
      child_path <- append_record(record_path, field, row)
      walk_frame_row(
        value,
        row = row,
        resource = resource,
        field = field,
        record = record,
        record_path = child_path,
        record_collection = field,
        record_index = row
      )
    }
    invisible(NULL)
  }

  walk_value <- function(
    value,
    resource,
    field,
    record,
    record_path,
    record_collection = NA_character_,
    record_index = NA_integer_,
    from_list_column = FALSE
  ) {
    if (is.null(value)) {
      return(invisible(NULL))
    }
    if (is.data.frame(value)) {
      if (from_list_column) {
        walk_record_frame(
          value,
          resource = resource,
          field = field,
          record = record,
          record_path = record_path
        )
      } else if (nrow(value) > 0L) {
        for (row in seq_len(nrow(value))) {
          walk_frame_row(
            value,
            row = row,
            resource = resource,
            field = field,
            record = record,
            record_path = record_path,
            record_collection = record_collection,
            record_index = record_index
          )
        }
      }
      return(invisible(NULL))
    }
    if (is.list(value)) {
      child_names <- names(value)
      named <- !is.null(child_names) &&
        length(child_names) == length(value) &&
        all(nzchar(child_names))
      if (named) {
        return(walk_named_object(
          value,
          resource = resource,
          field = field,
          record = record,
          record_path = record_path,
          record_collection = record_collection,
          record_index = record_index
        ))
      }
      scalar_array <- length(value) > 0L &&
        all(vapply(
          value,
          function(child) is.atomic(child) && !is.object(child),
          logical(1)
        ))
      if (scalar_array) {
        add_leaf(
          resource,
          record,
          field,
          unlist(value, use.names = FALSE),
          record_path,
          record_collection,
          record_index,
          force_array = TRUE
        )
        return(invisible(NULL))
      }
      for (i in seq_along(value)) {
        child <- value[[i]]
        if (is.atomic(child) && !is.object(child)) {
          add_leaf(
            resource,
            record,
            field,
            child,
            record_path,
            record_collection,
            record_index,
            force_array = TRUE
          )
        } else {
          child_path <- append_record(record_path, field, i)
          if (is.data.frame(child)) {
            for (row in seq_len(nrow(child))) {
              walk_frame_row(
                child,
                row = row,
                resource = resource,
                field = field,
                record = record,
                record_path = child_path,
                record_collection = field,
                record_index = i
              )
            }
          } else if (is.list(child) && !is.null(names(child))) {
            walk_named_object(
              child,
              resource = resource,
              field = field,
              record = record,
              record_path = child_path,
              record_collection = field,
              record_index = i
            )
          } else {
            walk_value(
              child,
              resource = resource,
              field = field,
              record = record,
              record_path = child_path,
              record_collection = field,
              record_index = i
            )
          }
        }
      }
      return(invisible(NULL))
    }
    if (is.atomic(value)) {
      add_leaf(
        resource,
        record,
        field,
        value,
        record_path,
        record_collection,
        record_index,
        force_array = from_list_column
      )
    }
    invisible(NULL)
  }

  walk_resource <- function(value, resource, field = "", root_prefix = "") {
    if (is.null(value)) {
      return(invisible(NULL))
    }
    if (is.data.frame(value)) {
      if (nrow(value) == 0L) {
        return(invisible(NULL))
      }
      for (row in seq_len(nrow(value))) {
        root_path <- paste0(".root[", root_prefix, row, "]")
        walk_frame_row(
          value,
          row = row,
          resource = resource,
          field = field,
          record = row,
          record_path = root_path
        )
      }
      return(invisible(NULL))
    }
    child_names <- names(value)
    named <- is.list(value) &&
      !is.null(child_names) &&
      length(child_names) == length(value) &&
      all(nzchar(child_names))
    file_map <- named &&
      any(grepl(
        "[/\\\\]|\\.(json|ya?ml|csv|tsv)$",
        child_names,
        ignore.case = TRUE
      ))
    if (file_map) {
      for (i in seq_along(value)) {
        source <- child_names[[i]]
        walk_resource(
          value[[i]],
          resource = resource,
          field = append_field(field, source),
          root_prefix = paste0(source, ":")
        )
      }
    } else if (named) {
      walk_named_object(
        value,
        resource = resource,
        field = field,
        record = NA_integer_,
        record_path = paste0(".root[", root_prefix, "object]")
      )
    } else if (is.list(value)) {
      for (i in seq_along(value)) {
        child <- value[[i]]
        root_path <- paste0(".root[", root_prefix, i, "]")
        if (is.data.frame(child)) {
          for (row in seq_len(nrow(child))) {
            walk_frame_row(
              child,
              row = row,
              resource = resource,
              field = field,
              record = i,
              record_path = root_path
            )
          }
        } else if (is.list(child) && !is.null(names(child))) {
          walk_named_object(
            child,
            resource = resource,
            field = field,
            record = i,
            record_path = root_path
          )
        } else {
          walk_value(
            child,
            resource = resource,
            field = field,
            record = i,
            record_path = root_path
          )
        }
      }
    } else {
      add_leaf(
        resource,
        NA_integer_,
        field,
        value,
        paste0(".root[", root_prefix, "object]")
      )
    }
    invisible(NULL)
  }

  for (resource in names(metadata)) {
    walk_resource(metadata[[resource]], resource)
  }
  if (row_index == 0L) {
    return(tibble::tibble(
      resource = character(),
      record = integer(),
      field = character(),
      value = character(),
      context = character(),
      .record_path = character(),
      .record_collection = character(),
      .record_index = integer(),
      .value_index = integer(),
      .structural_path = character()
    ))
  }
  tibble::tibble(
    resource = unlist(rows$resource, use.names = FALSE),
    record = unlist(rows$record, use.names = FALSE),
    field = unlist(rows$field, use.names = FALSE),
    value = unlist(rows$value, use.names = FALSE),
    context = unlist(rows$context, use.names = FALSE),
    .record_path = unlist(rows$.record_path, use.names = FALSE),
    .record_collection = unlist(
      rows$.record_collection,
      use.names = FALSE
    ),
    .record_index = unlist(rows$.record_index, use.names = FALSE),
    .value_index = unlist(rows$.value_index, use.names = FALSE),
    .structural_path = unlist(rows$.structural_path, use.names = FALSE)
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
    return(tibble::tibble(
      dataset_id = character(),
      file_group_id = character(),
      study_id = character(),
      participant_id = character(),
      role = character(),
      data_state = character(),
      device_id = character(),
      device_location = character(),
      device_location_type = character(),
      modalities = character(),
      modality_values = list(),
      format = character(),
      timezone = character(),
      file_count = integer(),
      available_file_count = integer(),
      variable_count = integer(),
      variable_names = list(),
      variable_terms = list()
    ))
  }

  keys <- paste(files$dataset_id, files$file_group_id, sep = "\r")
  unique_keys <- unique(keys)
  rows <- lapply(unique_keys, function(key) {
    index <- which(keys == key)
    group_id <- files$file_group_id[[index[[1L]]]]
    group_variables <- variables[
      !is.na(variables$file_group_id) &
        variables$file_group_id == group_id,
      ,
      drop = FALSE
    ]
    modality_values <- glc_explorer_file_group_field_values(
      unlist(files$modalities[index], use.names = FALSE)
    )
    device_location <- if ("device_location" %in% names(files)) {
      as.character(files$device_location[[index[[1L]]]])
    } else {
      NA_character_
    }
    device_location_type <- if ("device_location_type" %in% names(files)) {
      as.character(files$device_location_type[[index[[1L]]]])
    } else {
      NA_character_
    }
    tibble::tibble(
      dataset_id = files$dataset_id[[index[[1L]]]],
      file_group_id = group_id,
      study_id = files$study_id[[index[[1L]]]],
      participant_id = files$participant_id[[index[[1L]]]],
      role = files$role[[index[[1L]]]],
      data_state = files$data_state[[index[[1L]]]],
      device_id = files$device_id[[index[[1L]]]],
      device_location = device_location,
      device_location_type = device_location_type,
      modalities = glc_explorer_display_values(modality_values),
      modality_values = list(modality_values),
      format = files$format[[index[[1L]]]],
      timezone = files$timezone[[index[[1L]]]],
      file_count = length(index),
      available_file_count = sum(files$available[index] %in% TRUE),
      variable_count = nrow(group_variables),
      variable_names = list(glc_explorer_file_group_field_values(
        group_variables$name
      )),
      variable_terms = list(glc_explorer_file_group_field_values(
        group_variables$term
      ))
    )
  })
  dplyr::bind_rows(rows)
}

glc_explorer_file_group_field_values <- function(value) {
  value <- unique(as.character(value %||% character()))
  sort(value[!is.na(value) & nzchar(value)])
}

glc_explorer_file_group_field_choices <- function(file_groups) {
  list_values <- function(column) {
    if (!column %in% names(file_groups)) {
      return(character())
    }
    glc_explorer_file_group_field_values(
      unlist(file_groups[[column]], use.names = FALSE)
    )
  }
  values <- function(column) {
    if (!column %in% names(file_groups)) {
      return(character())
    }
    glc_explorer_file_group_field_values(file_groups[[column]])
  }
  list(
    device_id = values("device_id"),
    device_location = values("device_location"),
    location_type = values("device_location_type"),
    modality = list_values("modality_values"),
    role = values("role"),
    state = values("data_state"),
    variable = list_values("variable_names"),
    term = list_values("variable_terms")
  )
}

glc_explorer_file_group_matches_values <- function(values, selected) {
  selected <- glc_explorer_file_group_field_values(selected)
  if (length(selected) == 0L) {
    return(rep(TRUE, length(values)))
  }
  values <- as.character(values)
  !is.na(values) & nzchar(values) & values %in% selected
}

glc_explorer_file_group_matches_list_values <- function(values, selected) {
  selected <- glc_explorer_file_group_field_values(selected)
  if (length(selected) == 0L) {
    return(rep(TRUE, length(values)))
  }
  vapply(
    values,
    function(value) {
      any(glc_explorer_file_group_field_values(value) %in% selected)
    },
    logical(1)
  )
}

glc_explorer_filter_file_groups <- function(
  file_groups,
  dataset_ids = "all",
  query = "",
  device_ids = character(),
  device_locations = character(),
  location_types = character(),
  modalities = character(),
  roles = character(),
  states = character(),
  variable_names = character(),
  terms = character()
) {
  keep <- rep(TRUE, nrow(file_groups))
  dataset_ids <- unique(as.character(dataset_ids))
  dataset_ids <- dataset_ids[!is.na(dataset_ids) & nzchar(dataset_ids)]
  if (!"all" %in% dataset_ids) {
    keep <- keep & file_groups$dataset_id %in% dataset_ids
  }
  keep <- keep &
    glc_explorer_file_group_matches_values(
      file_groups$device_id,
      device_ids
    ) &
    glc_explorer_file_group_matches_values(
      file_groups$device_location,
      device_locations
    ) &
    glc_explorer_file_group_matches_values(
      file_groups$device_location_type,
      location_types
    ) &
    glc_explorer_file_group_matches_list_values(
      file_groups$modality_values,
      modalities
    ) &
    glc_explorer_file_group_matches_values(file_groups$role, roles) &
    glc_explorer_file_group_matches_values(file_groups$data_state, states) &
    glc_explorer_file_group_matches_list_values(
      file_groups$variable_names,
      variable_names
    ) &
    glc_explorer_file_group_matches_list_values(
      file_groups$variable_terms,
      terms
    )
  variable_text <- vapply(
    file_groups$variable_names,
    glc_explorer_display_values,
    character(1)
  )
  term_text <- vapply(
    file_groups$variable_terms,
    glc_explorer_display_values,
    character(1)
  )
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
      file_groups$device_location,
      file_groups$device_location_type,
      file_groups$modalities,
      file_groups$format,
      file_groups$timezone,
      variable_text,
      term_text
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
    `Wearing position` = display(file_groups$device_location),
    `Location type` = display(file_groups$device_location_type),
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

glc_explorer_file_group_compatibility <- function(groups, file_group_ids) {
  file_group_ids <- glc_explorer_nonempty_values(file_group_ids)
  if (length(file_group_ids) == 0L) {
    return(list(
      state = "zero",
      ok = FALSE,
      issues = character(),
      file_group_ids = character(),
      group_count = 0L,
      resolved_count = 0L,
      dataset_count = 0L,
      unresolved_ids = character()
    ))
  }

  group_rows <- match(file_group_ids, groups$file_group_id)
  unresolved_ids <- file_group_ids[is.na(group_rows)]
  selected_groups <- groups[
    group_rows[!is.na(group_rows)],
    ,
    drop = FALSE
  ]
  compatibility <- glc_explorer_selection_compatibility(
    selected_groups,
    variable_names = character(),
    variable_terms = character()
  )
  issues <- compatibility$issues
  if (length(unresolved_ids) > 0L) {
    unresolved_issue <- if (length(unresolved_ids) == 1L) {
      paste0(
        "One selected file group is not available in the package ",
        "compatibility inventory."
      )
    } else {
      sprintf(
        paste0(
          "%d selected file groups are not available in the package ",
          "compatibility inventory."
        ),
        length(unresolved_ids)
      )
    }
    issues <- c(issues, unresolved_issue)
  }
  issues <- glc_explorer_nonempty_values(issues)
  ok <- isTRUE(compatibility$ok) && length(unresolved_ids) == 0L

  list(
    state = if (ok) "compatible" else "incompatible",
    ok = ok,
    issues = issues,
    file_group_ids = file_group_ids,
    group_count = length(file_group_ids),
    resolved_count = nrow(selected_groups),
    dataset_count = length(glc_explorer_nonempty_values(
      selected_groups$dataset_id
    )),
    unresolved_ids = unresolved_ids
  )
}

glc_explorer_file_group_compatibility_tag <- function(
  compatibility,
  action_id
) {
  state <- compatibility$state %||% "zero"
  group_count <- compatibility$group_count %||% 0L
  dataset_count <- compatibility$dataset_count %||% 0L
  group_label <- if (identical(group_count, 1L)) {
    "file group"
  } else {
    "file groups"
  }
  dataset_label <- if (identical(dataset_count, 1L)) {
    "dataset"
  } else {
    "datasets"
  }

  if (identical(state, "zero")) {
    return(shiny::tags$div(
      class = "mb-3",
      role = "status",
      `aria-live` = "polite",
      `aria-atomic` = "true",
      shiny::tags$button(
        type = "button",
        class = "btn btn-secondary w-100 disabled",
        disabled = NA,
        shiny::icon("circle-info"),
        " No matching file groups"
      ),
      shiny::tags$p(
        class = "small text-body-secondary mt-1 mb-0",
        "Adjust the filters to choose groups for handoff."
      )
    ))
  }

  compatible <- identical(state, "compatible")
  action_label <- if (compatible) {
    if (identical(group_count, 1L)) {
      "1 group ready for handoff"
    } else {
      sprintf("%d groups ready for handoff", group_count)
    }
  } else {
    "Want to import these files? Filter them first"
  }

  shiny::tags$div(
    class = "mb-3",
    role = "status",
    `aria-live` = "polite",
    `aria-atomic` = "true",
    shiny::actionButton(
      action_id,
      action_label,
      icon = shiny::icon(if (compatible) "arrow-right" else "circle-info"),
      class = paste(
        "w-100",
        if (compatible) "btn-success" else "btn-warning"
      )
    ),
    shiny::tags$p(
      class = "small text-body-secondary mt-1 mb-0",
      sprintf(
        "%s across %d %s.",
        if (compatible) "Compatible selection" else
          "Selection needs refinement",
        dataset_count,
        dataset_label
      )
    )
  )
}

glc_explorer_file_group_compatibility_modal <- function(compatibility) {
  issues <- glc_explorer_nonempty_values(compatibility$issues)
  visible_issues <- utils::head(issues, 6L)
  remaining_issues <- max(0L, length(issues) - length(visible_issues))
  shiny::modalDialog(
    title = "Refine file groups before handoff",
    easyClose = TRUE,
    footer = shiny::modalButton("Continue filtering"),
    shiny::tags$p(
      sprintf(
        paste0(
          "%d matching file groups across %d datasets cannot yet be ",
          "collected as one compatible selection."
        ),
        compatibility$group_count %||% 0L,
        compatibility$dataset_count %||% 0L
      )
    ),
    if (length(visible_issues) > 0L) {
      shiny::tags$ul(lapply(visible_issues, shiny::tags$li))
    },
    if (remaining_issues > 0L) {
      shiny::tags$p(
        class = "small text-body-secondary",
        sprintf(
          "And %d more %s.",
          remaining_issues,
          if (identical(remaining_issues, 1L)) "issue" else "issues"
        )
      )
    },
    shiny::tags$p(
      class = "mb-0",
      paste(
        "Narrow the File groups filters until the button turns green.",
        "The green action transfers the exact visible selection to",
        "Select & hand off."
      )
    )
  )
}

glc_explorer_file_group_navigation_selection <- function(request, datasets) {
  requested <- if (is.list(request)) {
    glc_explorer_nonempty_values(request$dataset_ids)
  } else {
    character()
  }
  if (
    length(requested) == 0L ||
      !inherits(datasets, "data.frame") ||
      !"dataset_id" %in% names(datasets)
  ) {
    return("all")
  }

  available <- glc_explorer_nonempty_values(datasets$dataset_id)
  selected <- requested[requested %in% available]
  if (length(selected) == 0L) "all" else selected
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

glc_explorer_metadata_entry_key <- function(metadata) {
  if (nrow(metadata) == 0L) {
    return(character())
  }
  record <- ifelse(
    is.na(metadata$record),
    metadata$context %||% "object",
    as.character(metadata$record)
  )
  datasheet_path <- metadata$datasheet_path %||%
    rep(NA_character_, nrow(metadata))
  is_datasheet <- metadata$resource == "device_datasheets" &
    !is.na(datasheet_path) &
    nzchar(datasheet_path)
  entry <- record
  entry[is_datasheet] <- datasheet_path[is_datasheet]
  paste(metadata$resource, entry, sep = "\r")
}

glc_explorer_add_metadata_hierarchy_ids <- function(
  metadata,
  universe = metadata
) {
  if (is.null(metadata)) {
    return(NULL)
  }
  metadata$.entry_key <- glc_explorer_metadata_entry_key(metadata)
  universe_entry_keys <- unique(glc_explorer_metadata_entry_key(universe))
  metadata$.entry_id <- sprintf(
    "entry_%06d",
    match(metadata$.entry_key, universe_entry_keys)
  )
  universe_resources <- unique(universe$resource)
  metadata$.resource_id <- sprintf(
    "resource_%03d",
    match(metadata$resource, universe_resources)
  )
  metadata
}

glc_explorer_complete_metadata_entries <- function(metadata, matches) {
  if (is.null(metadata) || is.null(matches)) {
    return(NULL)
  }
  metadata <- glc_explorer_add_metadata_hierarchy_ids(metadata)
  matches <- glc_explorer_add_metadata_hierarchy_ids(
    matches,
    universe = metadata
  )
  matching_entries <- unique(matches$.entry_key)
  metadata[
    metadata$.entry_key %in% matching_entries,
    ,
    drop = FALSE
  ]
}

glc_explorer_metadata_record_label <- function(metadata) {
  if (nrow(metadata) == 0L) {
    return("Metadata entry")
  }
  first_value <- function(value) {
    value <- glc_compact_character(value)
    if (length(value) == 0L) NULL else value[[1L]]
  }

  resource <- metadata$resource[[1L]]
  if (identical(resource, "device_datasheets")) {
    identifier <- first_value(metadata$record_id %||% character())
    if (is.null(identifier)) {
      path <- first_value(metadata$datasheet_path %||% character())
      identifier <- if (is.null(path)) NULL else basename(path)
    }
    return(
      if (is.null(identifier)) {
        "Datasheet"
      } else {
        paste("Datasheet", identifier, sep = " \u2014 ")
      }
    )
  }

  context <- first_value(metadata$context)
  identifier <- first_value(metadata$record_id %||% character())
  descriptor <- NULL
  if (identical(resource, "participant_characteristics")) {
    field_leaf <- sub("^.*\\.", "", metadata$field)
    descriptor <- first_value(
      metadata$value[field_leaf == "participant_characteristic_name"]
    )
  }
  parts <- glc_compact_character(c(context, identifier, descriptor))
  parts <- parts[!duplicated(parts)]
  if (length(parts) == 0L) "Metadata entry" else
    paste(parts, collapse = " \u2014 ")
}

glc_explorer_metadata_entry_index <- function(metadata) {
  if (is.null(metadata) || nrow(metadata) == 0L) {
    return(tibble::tibble(
      resource = character(),
      resource_id = character(),
      entry_id = character(),
      label = character(),
      value_count = integer(),
      is_flat = logical()
    ))
  }
  if (!all(c(".entry_id", ".resource_id", ".entry_key") %in% names(metadata))) {
    metadata <- glc_explorer_add_metadata_hierarchy_ids(metadata)
  }
  entry_ids <- unique(metadata$.entry_id)
  entry_rows <- split(
    seq_len(nrow(metadata)),
    factor(metadata$.entry_id, levels = entry_ids)
  )
  rows <- lapply(seq_along(entry_ids), function(index) {
    entry_id <- entry_ids[[index]]
    entry <- metadata[entry_rows[[index]], , drop = FALSE]
    tibble::tibble(
      resource = entry$resource[[1L]],
      resource_id = entry$.resource_id[[1L]],
      entry_id = entry_id,
      label = glc_explorer_metadata_record_label(entry),
      value_count = nrow(entry),
      is_flat = !identical(entry$resource[[1L]], "device_datasheets") &&
        !any(grepl(".", entry$field, fixed = TRUE))
    )
  })
  dplyr::bind_rows(rows)
}

glc_explorer_metadata_resource_index <- function(metadata, entries = NULL) {
  if (is.null(entries)) {
    entries <- glc_explorer_metadata_entry_index(metadata)
  }
  if (nrow(entries) == 0L) {
    return(tibble::tibble(
      resource = character(),
      resource_id = character(),
      entry_count = integer(),
      value_count = integer()
    ))
  }
  resources <- unique(entries$resource_id)
  rows <- lapply(resources, function(resource_id) {
    resource_entries <- entries[entries$resource_id == resource_id, ]
    tibble::tibble(
      resource = resource_entries$resource[[1L]],
      resource_id = resource_id,
      entry_count = nrow(resource_entries),
      value_count = sum(resource_entries$value_count)
    )
  })
  dplyr::bind_rows(rows)
}

glc_explorer_metadata_entry_slice <- function(
  metadata,
  limit = 25L
) {
  if (is.null(metadata)) {
    return(NULL)
  }
  entries <- glc_explorer_metadata_entry_index(metadata)
  limit <- suppressWarnings(as.integer(limit[[1L]] %||% 25L))
  if (is.na(limit) || limit < 1L) {
    limit <- 25L
  }
  loaded_count <- min(limit, nrow(entries))
  loaded_entries <- utils::head(entries, loaded_count)
  loaded_ids <- loaded_entries$entry_id
  data <- metadata[
    metadata$.entry_id %in% loaded_ids,
    ,
    drop = FALSE
  ]
  list(
    data = data,
    entries = loaded_entries,
    loaded_entry_count = loaded_count,
    total_entry_count = nrow(entries),
    loaded_value_count = nrow(data),
    total_value_count = nrow(metadata),
    remaining_entry_count = nrow(entries) - loaded_count
  )
}

glc_explorer_metadata_scalar_tag <- function(metadata) {
  fields <- unique(metadata$field)
  counts <- vapply(
    fields,
    function(field) sum(metadata$field == field),
    integer(1)
  )
  scalar_fields <- fields[counts == 1L]
  repeated_fields <- fields[counts > 1L]
  scalar_values <- shiny::tagList(lapply(scalar_fields, function(field) {
    value <- as.character(metadata$value[metadata$field == field][[1L]])
    if (is.na(value) || !nzchar(value)) {
      value <- "\u2014"
    }
    shiny::tagList(
      shiny::tags$dt(
        class = "fw-normal",
        shiny::tags$code(gsub(".", " \u203a ", field, fixed = TRUE))
      ),
      shiny::tags$dd(class = "text-break", value)
    )
  }))
  scalar_grid <- if (length(scalar_fields) > 0L) {
    shiny::tags$dl(
      class = "small mb-2 metadata-values-grid",
      scalar_values
    )
  }
  repeated <- shiny::tagList(lapply(repeated_fields, function(field) {
    glc_explorer_metadata_repeated_field_tag(
      gsub(".", " \u203a ", field, fixed = TRUE),
      metadata$value[metadata$field == field]
    )
  }))
  shiny::tags$div(
    class = "metadata-record-values",
    `aria-label` = "Metadata values",
    scalar_grid,
    repeated
  )
}

glc_explorer_metadata_repeated_field_tag <- function(field, values) {
  values <- as.character(values)
  values[is.na(values) | !nzchar(values)] <- "\u2014"
  count <- length(values)
  shiny::tags$details(
    class = "metadata-repeated-field mb-2",
    shiny::tags$summary(
      class = "py-1",
      shiny::tags$span(
        class = "metadata-disclosure-arrow",
        `aria-hidden` = "true",
        shiny::icon("chevron-right")
      ),
      shiny::tags$code(field),
      shiny::tags$span(
        class = "badge text-bg-light",
        sprintf("%d item%s", count, if (count == 1L) "" else "s")
      )
    ),
    shiny::tags$ol(
      class = "small mb-2 mt-1 metadata-repeated-values",
      lapply(values, function(value) {
        shiny::tags$li(class = "text-break", value)
      })
    )
  )
}

glc_explorer_metadata_ensure_structure <- function(metadata) {
  required <- c(
    ".record_path",
    ".record_collection",
    ".record_index",
    ".value_index",
    ".structural_path"
  )
  if (all(required %in% names(metadata))) {
    return(metadata)
  }
  record <- ifelse(
    is.na(metadata$record),
    "object",
    as.character(metadata$record)
  )
  path <- paste0(".root[", record, "]")
  datasheet_path <- metadata$datasheet_path %||%
    rep(NA_character_, nrow(metadata))
  datasheet <- metadata$resource == "device_datasheets" &
    !is.na(datasheet_path) &
    nzchar(datasheet_path)
  path[datasheet] <- paste0(".root[", datasheet_path[datasheet], "]")
  metadata$.record_path <- path
  metadata$.record_collection <- NA_character_
  metadata$.record_index <- NA_integer_
  if (any(datasheet)) {
    relative_field <- metadata$field
    prefix <- paste0(datasheet_path, ".")
    prefixed <- datasheet & startsWith(relative_field, prefix)
    relative_field[prefixed] <- substring(
      relative_field[prefixed],
      nchar(prefix[prefixed]) + 1L
    )
    nested_datasheet <- datasheet &
      !is.na(metadata$record) &
      grepl(".", relative_field, fixed = TRUE)
    collection <- sub("\\..*$", "", relative_field)
    metadata$.record_collection[nested_datasheet] <- paste(
      datasheet_path[nested_datasheet],
      collection[nested_datasheet],
      sep = "."
    )
    metadata$.record_index[nested_datasheet] <- metadata$record[
      nested_datasheet
    ]
    metadata$.record_path[nested_datasheet] <- paste(
      path[nested_datasheet],
      paste0(
        metadata$.record_collection[nested_datasheet],
        "[",
        metadata$.record_index[nested_datasheet],
        "]"
      ),
      sep = "\r"
    )
  }
  metadata$.value_index <- NA_integer_
  key <- paste(
    metadata$resource,
    metadata$.record_path,
    metadata$field,
    sep = "\r"
  )
  for (field_key in unique(key)) {
    rows <- which(key == field_key)
    if (length(rows) > 1L) {
      metadata$.value_index[rows] <- seq_along(rows)
    }
  }
  leaf <- metadata$field
  indexed <- !is.na(metadata$.value_index)
  leaf[indexed] <- paste0(
    leaf[indexed],
    "[",
    metadata$.value_index[indexed],
    "]"
  )
  metadata$.structural_path <- paste(
    metadata$.record_path,
    leaf,
    sep = "\r"
  )
  metadata
}

glc_explorer_metadata_child_records <- function(metadata, parent_path) {
  metadata <- glc_explorer_metadata_ensure_structure(metadata)
  prefix <- paste0(parent_path, "\r")
  paths <- unique(metadata$.record_path)
  paths <- paths[startsWith(paths, prefix)]
  if (length(paths) == 0L) {
    return(tibble::tibble(
      path = character(),
      collection = character(),
      index = integer()
    ))
  }
  remainder <- substring(paths, nchar(prefix) + 1L)
  segment <- sub("\r.*$", "", remainder)
  child_path <- paste(parent_path, segment, sep = "\r")
  keep <- !duplicated(child_path)
  segment <- segment[keep]
  child_path <- child_path[keep]
  index <- suppressWarnings(as.integer(sub(
    "^.*\\[([0-9]+)\\]$",
    "\\1",
    segment
  )))
  collection <- sub("\\[[0-9]+\\]$", "", segment)
  tibble::tibble(
    path = child_path,
    collection = collection,
    index = index
  )
}

glc_explorer_metadata_path_rows <- function(
  metadata,
  path,
  descendants = FALSE
) {
  if (descendants) {
    metadata$.record_path == path |
      startsWith(metadata$.record_path, paste0(path, "\r"))
  } else {
    metadata$.record_path == path
  }
}

glc_explorer_metadata_relative_fields <- function(metadata, collection) {
  if (is.null(collection) || is.na(collection) || !nzchar(collection)) {
    return(metadata)
  }
  prefix <- paste0(collection, ".")
  relative <- startsWith(metadata$field, prefix)
  metadata$field[relative] <- substring(
    metadata$field[relative],
    nchar(prefix) + 1L
  )
  metadata
}

glc_explorer_metadata_record_kind <- function(collection) {
  name <- sub("^.*\\.", "", collection)
  switch(
    name,
    study_contributors = "Contributor",
    study_groups = "Study group",
    device_sensors = "Sensor",
    dataset_variable_terms = "Variable term",
    dataset_file = "Dataset file",
    dataset_file_variables = "Variable",
    dataset_file_variables_factor_levels = "Factor level",
    datasheet_calibration_parameters = "Calibration parameter",
    datasheet_calibration_spectral_sensitivity = "Calibration point",
    datasheet_channel = "Channel",
    {
      label <- glc_explorer_humanize_metadata_name(name)
      sub("s$", "", label)
    }
  )
}

glc_explorer_metadata_collection_label <- function(collection) {
  name <- sub("^.*\\.", "", collection)
  switch(
    name,
    dataset_file = "Dataset Files",
    dataset_file_variables = "Variables",
    dataset_file_variables_factor_levels = "Factor Levels",
    datasheet_calibration_parameters = "Calibration Parameters",
    datasheet_channel = "Channels",
    glc_explorer_humanize_metadata_name(name)
  )
}

glc_explorer_metadata_nested_record_label <- function(
  metadata,
  path,
  collection,
  index
) {
  direct <- metadata[
    glc_explorer_metadata_path_rows(metadata, path),
    ,
    drop = FALSE
  ]
  field <- sub("^.*\\.", "", direct$field)
  number_field <- grepl("(_nr|_number)$", field)
  display_index <- index
  if (any(number_field)) {
    number_value <- glc_compact_character(direct$value[number_field])
    if (length(number_value) > 0L) {
      display_index <- number_value[[1L]]
    }
  }
  priority <- c(
    "contributor_full_name",
    "study_group_name",
    "dataset_file_names",
    "dataset_file_variables_name",
    "term",
    "device_sensor_type",
    "datasheet_channel_name",
    "parameter_name",
    "label",
    "value"
  )
  score <- match(field, priority)
  fallback <- is.na(score) &
    grepl(
      paste0(
        "(^|_)(internal_id|id|name|title|label|term|nr|number|",
        "wavelength|type|model)$"
      ),
      field
    )
  score[fallback] <- length(priority) + seq_len(sum(fallback))
  candidates <- which(!is.na(score))
  descriptor <- NULL
  if (length(candidates) > 0L) {
    candidates <- candidates[order(score[candidates])]
    values <- as.character(direct$value[candidates])
    usable <- !is.na(values) & nzchar(values)
    if (any(usable)) {
      selected <- candidates[which(usable)[[1L]]]
      descriptor <- values[which(usable)[[1L]]]
      if (endsWith(field[[selected]], "_wavelength")) {
        descriptor <- paste("wavelength:", descriptor)
      }
    }
  }
  label <- paste(
    glc_explorer_metadata_record_kind(collection),
    display_index
  )
  if (!is.null(descriptor)) paste(label, descriptor, sep = " \u2014 ") else
    label
}

glc_explorer_metadata_field_values_tag <- function(metadata) {
  if (nrow(metadata) == 0L) {
    return(NULL)
  }
  nested <- grepl(".", metadata$field, fixed = TRUE)
  if (!any(nested)) {
    return(glc_explorer_metadata_scalar_tag(metadata))
  }
  shiny::tagList(
    if (any(!nested)) {
      glc_explorer_metadata_scalar_tag(metadata[!nested, , drop = FALSE])
    },
    glc_explorer_metadata_branch_tag(metadata[nested, , drop = FALSE])
  )
}

glc_explorer_metadata_record_node_tag <- function(
  metadata,
  path,
  collection = NULL
) {
  direct <- metadata[
    glc_explorer_metadata_path_rows(metadata, path),
    ,
    drop = FALSE
  ]
  direct <- glc_explorer_metadata_relative_fields(direct, collection)
  children <- glc_explorer_metadata_child_records(metadata, path)
  collections <- unique(children$collection)
  child_tags <- shiny::tagList(lapply(collections, function(child_collection) {
    records <- children[
      children$collection == child_collection,
      ,
      drop = FALSE
    ]
    record_tags <- shiny::tagList(lapply(seq_len(nrow(records)), function(row) {
      record <- records[row, ]
      value_rows <- glc_explorer_metadata_path_rows(
        metadata,
        record$path,
        descendants = TRUE
      )
      shiny::tags$details(
        class = "metadata-nested-record ms-3 mb-2",
        open = if (nrow(records) == 1L) NA else NULL,
        shiny::tags$summary(
          class = "py-1",
          shiny::icon("rectangle-list"),
          paste0(
            " ",
            glc_explorer_metadata_nested_record_label(
              metadata,
              record$path,
              child_collection,
              record$index
            )
          ),
          shiny::tags$span(
            class = "badge text-bg-light ms-2",
            sprintf(
              "%d value%s",
              sum(value_rows),
              if (sum(value_rows) == 1L) "" else "s"
            )
          )
        ),
        glc_explorer_metadata_record_node_tag(
          metadata,
          record$path,
          collection = child_collection
        )
      )
    }))
    shiny::tags$details(
      class = "metadata-record-collection ms-3 mb-2",
      open = if (nrow(records) == 1L) NA else NULL,
      shiny::tags$summary(
        class = "py-1 fw-semibold",
        shiny::icon("layer-group"),
        paste0(
          " ",
          glc_explorer_metadata_collection_label(child_collection)
        ),
        shiny::tags$span(
          class = "badge text-bg-light ms-2",
          sprintf(
            "%d record%s",
            nrow(records),
            if (nrow(records) == 1L) "" else "s"
          )
        )
      ),
      record_tags
    )
  }))
  shiny::tagList(
    glc_explorer_metadata_field_values_tag(direct),
    child_tags
  )
}

glc_explorer_metadata_entry_body_tag <- function(metadata) {
  if (nrow(metadata) == 0L) {
    return(NULL)
  }
  if (identical(metadata$resource[[1L]], "device_datasheets")) {
    return(glc_explorer_device_datasheet_hierarchy_tag(metadata))
  }
  metadata <- glc_explorer_metadata_ensure_structure(metadata)
  roots <- unique(sub("\r.*$", "", metadata$.record_path))
  shiny::tagList(lapply(roots, function(path) {
    glc_explorer_metadata_record_node_tag(metadata, path)
  }))
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
    branch_fields <- unique(leaf[index])
    field_counts <- vapply(
      branch_fields,
      function(field) sum(index & leaf == field),
      integer(1)
    )
    scalar_fields <- branch_fields[field_counts == 1L]
    repeated_fields <- branch_fields[field_counts > 1L]
    inline <- length(repeated_fields) == 0L &&
      !any(grepl(".", branch_fields, fixed = TRUE))
    if (inline) {
      values <- shiny::tagList(lapply(branch_fields, function(field) {
        row <- which(index & leaf == field)[[1L]]
        value <- as.character(metadata$value[[row]])
        if (is.na(value) || !nzchar(value)) {
          value <- "\u2014"
        }
        shiny::tagList(
          shiny::tags$dt(
            class = "fw-normal",
            shiny::tags$code(
              paste(branch_label, field, sep = " \u203a ")
            )
          ),
          shiny::tags$dd(class = "text-break", value)
        )
      }))
      return(shiny::tags$div(
        class = "metadata-inline-child",
        shiny::tags$dl(
          class = "small mb-2 metadata-values-grid",
          values
        )
      ))
    }
    scalar_values <- shiny::tagList(lapply(scalar_fields, function(field) {
      row <- which(index & leaf == field)[[1L]]
      value <- as.character(metadata$value[[row]])
      if (is.na(value) || !nzchar(value)) {
        value <- "\u2014"
      }
      shiny::tagList(
        shiny::tags$dt(
          class = "fw-normal",
          shiny::tags$code(field)
        ),
        shiny::tags$dd(class = "text-break", value)
      )
    }))
    scalar_grid <- if (length(scalar_fields) > 0L) {
      shiny::tags$dl(
        class = "small mb-2 metadata-values-grid",
        scalar_values
      )
    }
    repeated <- shiny::tagList(lapply(repeated_fields, function(field) {
      glc_explorer_metadata_repeated_field_tag(
        field,
        metadata$value[index & leaf == field]
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
      scalar_grid,
      repeated
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
        class = "fw-normal",
        shiny::tags$code(fields[[row]])
      ),
      shiny::tags$dd(
        class = "text-break",
        metadata$value[[row]]
      )
    )
  }))
  shiny::tags$dl(class = "small mb-2 metadata-values-grid", values)
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
  metadata <- glc_explorer_metadata_ensure_structure(metadata)
  record_ids <- metadata$record_id %||% character()
  record_ids <- unique(record_ids[!is.na(record_ids) & nzchar(record_ids)])
  datasheet_label <- if (length(record_ids) == 0L) {
    "Datasheet"
  } else {
    paste("Datasheet", record_ids[[1L]], sep = " \u2014 ")
  }
  path <- metadata$datasheet_path[[1L]]
  prefix <- paste0(path, ".")
  in_path <- !is.na(path) & startsWith(metadata$field, prefix)
  metadata$field[in_path] <- substring(
    metadata$field[in_path],
    nchar(prefix) + 1L
  )
  collection_path <- !is.na(metadata$.record_collection) &
    startsWith(metadata$.record_collection, prefix)
  metadata$.record_collection[collection_path] <- substring(
    metadata$.record_collection[collection_path],
    nchar(prefix) + 1L
  )
  if (!is.na(path) && nzchar(path)) {
    metadata$.record_path <- vapply(
      strsplit(metadata$.record_path, "\r", fixed = TRUE),
      function(segments) {
        nested <- seq_along(segments) > 1L &
          startsWith(segments, prefix)
        segments[nested] <- substring(segments[nested], nchar(prefix) + 1L)
        paste(segments, collapse = "\r")
      },
      character(1)
    )
  }
  roots <- unique(sub("\r.*$", "", metadata$.record_path))
  contents <- shiny::tagList(lapply(roots, function(root) {
    glc_explorer_metadata_record_node_tag(metadata, root)
  }))

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
    contents
  )
}

glc_explorer_device_datasheet_hierarchy_tag <- function(metadata) {
  metadata <- glc_explorer_metadata_ensure_structure(metadata)
  path <- metadata$datasheet_path %||% rep(NA_character_, nrow(metadata))
  missing_path <- is.na(path) | !nzchar(path)
  if (any(missing_path)) {
    inferred_path <- vapply(
      metadata$field[missing_path],
      glc_explorer_datasheet_path_from_field,
      character(1)
    )
    source_path <- grepl(
      "[/\\\\]|\\.(json|ya?ml|csv|tsv)$",
      inferred_path,
      ignore.case = TRUE
    )
    missing_rows <- which(missing_path)
    path[missing_rows[source_path]] <- inferred_path[source_path]

    embedded_rows <- missing_rows[!source_path]
    if (length(embedded_rows) > 0L) {
      root_path <- sub("\r.*$", "", metadata$.record_path)
      embedded_roots <- unique(root_path[embedded_rows])
      embedded_labels <- if (length(embedded_roots) == 1L) {
        "datasheet"
      } else {
        paste("datasheet", seq_along(embedded_roots))
      }
      path[embedded_rows] <- embedded_labels[
        match(root_path[embedded_rows], embedded_roots)
      ]
    }
  }
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
        shiny::icon("rectangle-list"),
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

glc_explorer_metadata_page <- function(
  metadata,
  page = 1L,
  page_size = 500L
) {
  if (is.null(metadata)) {
    return(NULL)
  }
  allowed_sizes <- c(100L, 500L, 1000L, 2500L)
  page_size <- suppressWarnings(as.integer(page_size[[1L]] %||% 500L))
  if (is.na(page_size) || !page_size %in% allowed_sizes) {
    page_size <- 500L
  }
  page <- suppressWarnings(as.integer(page[[1L]] %||% 1L))
  if (is.na(page) || page < 1L) {
    page <- 1L
  }

  total <- nrow(metadata)
  page_count <- max(1L, as.integer(ceiling(total / page_size)))
  page <- min(page, page_count)
  first <- if (total == 0L) 0L else (page - 1L) * page_size + 1L
  last <- min(total, page * page_size)
  rows <- if (total == 0L) {
    metadata[FALSE, , drop = FALSE]
  } else {
    metadata[seq.int(first, last), , drop = FALSE]
  }
  list(
    data = rows,
    page = page,
    page_count = page_count,
    page_size = page_size,
    total = total,
    first = first,
    last = last
  )
}

glc_explorer_metadata_page_message <- function(page) {
  if (is.null(page)) {
    return("No metadata loaded.")
  }
  if (page$total == 0L) {
    return("No metadata values match the filters.")
  }
  sprintf(
    paste0(
      "Showing %s\u2013%s of %s matching metadata value(s); ",
      "page %s of %s."
    ),
    format(page$first, big.mark = ",", scientific = FALSE),
    format(page$last, big.mark = ",", scientific = FALSE),
    format(page$total, big.mark = ",", scientific = FALSE),
    format(page$page, big.mark = ",", scientific = FALSE),
    format(page$page_count, big.mark = ",", scientific = FALSE)
  )
}

glc_explorer_inventory_page <- function(data, page = 1L, page_size = 100L) {
  glc_explorer_metadata_page(
    data,
    page = page,
    page_size = page_size
  )
}

glc_explorer_inventory_page_message <- function(page, item) {
  if (is.null(page)) {
    return(paste("No", item, "loaded."))
  }
  if (page$total == 0L) {
    return(paste("No", item, "match the filters."))
  }
  sprintf(
    "Showing %s\u2013%s of %s matching %s; page %s of %s.",
    format(page$first, big.mark = ",", scientific = FALSE),
    format(page$last, big.mark = ",", scientific = FALSE),
    format(page$total, big.mark = ",", scientific = FALSE),
    item,
    format(page$page, big.mark = ",", scientific = FALSE),
    format(page$page_count, big.mark = ",", scientific = FALSE)
  )
}

glc_explorer_inventory_pagination_tag <- function(
  page,
  input_prefix,
  item,
  ns = identity
) {
  if (is.null(page) || page$total == 0L || page$page_count <= 1L) {
    return(NULL)
  }
  previous <- shiny::actionButton(
    ns(paste0(input_prefix, "_previous")),
    "Previous",
    icon = shiny::icon("chevron-left"),
    class = "btn-sm btn-outline-secondary",
    `aria-label` = paste("Previous", item, "page")
  )
  next_page <- shiny::actionButton(
    ns(paste0(input_prefix, "_next")),
    "Next",
    icon = shiny::icon("chevron-right"),
    class = "btn-sm btn-outline-secondary",
    `aria-label` = paste("Next", item, "page")
  )
  if (page$page <= 1L) {
    previous$attribs$disabled <- "disabled"
  }
  if (page$page >= page$page_count) {
    next_page$attribs$disabled <- "disabled"
  }
  shiny::tags$nav(
    class = "d-flex align-items-center gap-2",
    `aria-label` = paste(tools::toTitleCase(item), "pages"),
    previous,
    shiny::tags$span(
      class = "small text-body-secondary text-nowrap",
      sprintf("Page %d of %d", page$page, page$page_count)
    ),
    next_page
  )
}

glc_explorer_metadata_pagination_tag <- function(page, ns = identity) {
  if (is.null(page) || page$total == 0L) {
    return(NULL)
  }
  previous <- shiny::actionButton(
    ns("metadata_previous"),
    "Previous",
    icon = shiny::icon("chevron-left"),
    class = "btn-sm btn-outline-secondary",
    `aria-label` = "Previous metadata page"
  )
  next_page <- shiny::actionButton(
    ns("metadata_next"),
    "Next",
    icon = shiny::icon("chevron-right"),
    class = "btn-sm btn-outline-secondary",
    `aria-label` = "Next metadata page"
  )
  if (page$page <= 1L) {
    previous$attribs$disabled <- "disabled"
  }
  if (page$page >= page$page_count) {
    next_page$attribs$disabled <- "disabled"
  }

  shiny::tags$nav(
    class = "d-flex align-items-center gap-2",
    `aria-label` = "Metadata pages",
    previous,
    shiny::tags$span(
      class = "small text-body-secondary text-nowrap",
      sprintf("Page %d of %d", page$page, page$page_count)
    ),
    next_page
  )
}

glc_explorer_metadata_lazy_output <- function(
  output_id,
  label,
  ns = identity
) {
  shiny::tags$div(
    class = "metadata-lazy-slot",
    shiny::uiOutput(
      ns(output_id),
      class = "metadata-lazy-output"
    ),
    shiny::tags$div(
      class = paste(
        "metadata-lazy-placeholder",
        "alert alert-info py-2 px-3 my-2"
      ),
      role = "status",
      `aria-live` = "polite",
      shiny::tags$div(
        class = "d-flex align-items-center gap-2",
        shiny::tags$span(
          class = "spinner-border spinner-border-sm flex-shrink-0",
          `aria-hidden` = "true"
        ),
        shiny::tags$span(label)
      ),
      shiny::tags$div(
        class = "progress mt-2",
        style = "height: 0.35rem;",
        role = "progressbar",
        `aria-label` = label,
        `aria-valuetext` = "In progress",
        shiny::tags$div(
          class = paste(
            "progress-bar progress-bar-striped",
            "progress-bar-animated bg-primary w-100"
          )
        )
      )
    )
  )
}

glc_explorer_metadata_hierarchy_message <- function(
  matches,
  metadata,
  entries = NULL,
  resources = NULL
) {
  if (is.null(matches) || is.null(metadata)) {
    return("No metadata loaded.")
  }
  if (nrow(matches) == 0L) {
    return("No metadata values match the filters.")
  }
  if (is.null(entries)) {
    entries <- glc_explorer_metadata_entry_index(metadata)
  }
  if (is.null(resources)) {
    resources <- glc_explorer_metadata_resource_index(
      metadata,
      entries = entries
    )
  }
  sprintf(
    paste0(
      "%s matching metadata value(s) in %s complete entr%s ",
      "across %s resource(s). Open a resource to load its records."
    ),
    format(nrow(matches), big.mark = ",", scientific = FALSE),
    format(nrow(entries), big.mark = ",", scientific = FALSE),
    if (nrow(entries) == 1L) "y" else "ies",
    format(nrow(resources), big.mark = ",", scientific = FALSE)
  )
}

glc_explorer_metadata_hierarchy_tag <- function(
  metadata,
  ns = identity,
  open_resource = NULL,
  resources = NULL
) {
  if (is.null(metadata) || nrow(metadata) == 0L) {
    return(shiny::tags$p(
      class = "text-body-secondary",
      "No metadata values match these filters."
    ))
  }

  if (is.null(resources)) {
    resources <- glc_explorer_metadata_resource_index(metadata)
  }
  panels <- lapply(seq_len(nrow(resources)), function(row) {
    resource <- resources[row, ]
    bslib::accordion_panel(
      value = resource$resource_id,
      icon = shiny::icon("folder-tree"),
      title = shiny::tagList(
        resource$resource,
        shiny::tags$span(
          class = "badge text-bg-secondary ms-2",
          sprintf(
            "%d entr%s \u00b7 %d value%s",
            resource$entry_count,
            if (resource$entry_count == 1L) "y" else "ies",
            resource$value_count,
            if (resource$value_count == 1L) "" else "s"
          )
        )
      ),
      glc_explorer_metadata_lazy_output(
        paste0("metadata_resource_body_", resource$resource_id),
        sprintf(
          "Loading %d complete metadata entr%s\u2026",
          resource$entry_count,
          if (resource$entry_count == 1L) "y" else "ies"
        ),
        ns = ns
      )
    )
  })
  selected <- resources$resource_id[resources$resource %in% open_resource]
  open <- if (length(selected) == 1L) selected else FALSE
  do.call(
    bslib::accordion,
    c(
      panels,
      list(
        id = ns("metadata_resources"),
        open = open,
        multiple = TRUE,
        class = "metadata-resource-hierarchy"
      )
    )
  )
}

glc_explorer_metadata_resource_body_tag <- function(
  slice,
  ns = identity,
  open_entries = FALSE,
  batch_size = 25L
) {
  if (is.null(slice) || slice$total_entry_count == 0L) {
    return(NULL)
  }
  if (length(open_entries) == 0L) {
    open_entries <- FALSE
  }
  status <- shiny::tags$p(
    class = "small text-body-secondary",
    role = "status",
    `aria-live` = "polite",
    sprintf(
      "Loaded %d of %d complete entr%s (%d of %d values).",
      slice$loaded_entry_count,
      slice$total_entry_count,
      if (slice$total_entry_count == 1L) "y" else "ies",
      slice$loaded_value_count,
      slice$total_value_count
    )
  )

  if (
    slice$total_entry_count == 1L &&
      isTRUE(slice$entries$is_flat[[1L]])
  ) {
    return(shiny::tagList(
      status,
      glc_explorer_metadata_entry_body_tag(slice$data)
    ))
  }

  panels <- lapply(seq_len(nrow(slice$entries)), function(row) {
    entry <- slice$entries[row, ]
    bslib::accordion_panel(
      value = entry$entry_id,
      icon = shiny::icon("rectangle-list"),
      title = shiny::tagList(
        entry$label,
        shiny::tags$span(
          class = "badge text-bg-light ms-2",
          sprintf(
            "%d value%s",
            entry$value_count,
            if (entry$value_count == 1L) "" else "s"
          )
        )
      ),
      glc_explorer_metadata_lazy_output(
        paste0("metadata_entry_body_", entry$entry_id),
        sprintf(
          "Loading %s metadata value%s\u2026",
          format(entry$value_count, big.mark = ",", scientific = FALSE),
          if (entry$value_count == 1L) "" else "s"
        ),
        ns = ns
      )
    )
  })
  records_id <- paste0(
    "metadata_records_",
    slice$entries$resource_id[[1L]]
  )
  records <- do.call(
    bslib::accordion,
    c(
      panels,
      list(
        id = ns(records_id),
        open = open_entries,
        multiple = TRUE,
        class = "metadata-entry-hierarchy"
      )
    )
  )

  more <- NULL
  if (slice$remaining_entry_count > 0L) {
    next_count <- min(batch_size, slice$remaining_entry_count)
    resource <- slice$entries$resource[[1L]]
    more <- shiny::actionButton(
      ns(paste0(
        "metadata_more_",
        slice$entries$resource_id[[1L]]
      )),
      sprintf(
        "Load next %d entr%s",
        next_count,
        if (next_count == 1L) "y" else "ies"
      ),
      icon = shiny::icon("plus"),
      class = "btn-sm btn-outline-secondary mt-2",
      `aria-label` = sprintf(
        "Load the next %d %s metadata entries",
        next_count,
        resource
      )
    )
  }
  shiny::tagList(status, records, more)
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
  file_group_filter <- function(input_id, label, placeholder) {
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

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      id = ns("sidebar"),
      width = 280,
      shiny::conditionalPanel(
        condition = "input.contents_tab === 'Metadata'",
        shiny::tags$h5("Metadata", class = "mb-3"),
        shiny::tags$div(
          class = "alert alert-info py-2 small",
          role = "note",
          shiny::icon("circle-info"),
          shiny::tags$strong(" Tip: "),
          paste(
            "Hierarchy loads complete records as you open them.",
            "Use Table to compare matching values across records."
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
        shiny::conditionalPanel(
          condition = "input.metadata_view === 'table'",
          shiny::selectInput(
            ns("metadata_page_size"),
            "Values per page",
            choices = c(
              "100" = 100L,
              "500" = 500L,
              "1,000" = 1000L,
              "2,500" = 2500L
            ),
            selected = 500L
          ),
          shiny::numericInput(
            ns("metadata_page"),
            "Page",
            value = 1L,
            min = 1L,
            max = 1L,
            step = 1L,
            width = "100%"
          ),
          ns = ns
        ),
        ns = ns
      ),
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
        shiny::uiOutput(ns("file_group_compatibility")),
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
          placeholder = "Group, field, variable, or term"
        ),
        file_group_filter(
          "file_group_device_id",
          "Device IDs",
          "Any device"
        ),
        file_group_filter(
          "file_group_device_location",
          "Wearing positions",
          "Any wearing position"
        ),
        file_group_filter(
          "file_group_location_type",
          "Location types",
          "Any location type"
        ),
        file_group_filter(
          "file_group_modality",
          "Modalities",
          "Any modality"
        ),
        file_group_filter(
          "file_group_role",
          "Roles",
          "Any role"
        ),
        file_group_filter(
          "file_group_state",
          "Data states",
          "Any data state"
        ),
        file_group_filter(
          "file_group_variable",
          "Contained variables",
          "Any variable"
        ),
        file_group_filter(
          "file_group_term",
          "Semantic terms",
          "Any semantic term"
        ),
        shiny::helpText(
          paste(
            "Choices within a field use OR; active fields combine with AND.",
            "Search also covers variables and semantic terms."
          )
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
      )
    ),
    shiny::uiOutput(ns("status_message")),
    bslib::navset_card_tab(
      id = ns("contents_tab"),
      full_screen = TRUE,
      bslib::nav_panel(
        "Metadata",
        shiny::tags$div(
          class = paste(
            "d-flex flex-wrap align-items-center",
            "justify-content-between gap-2 mb-3"
          ),
          shiny::tags$p(
            class = "text-body-secondary mb-0",
            shiny::textOutput(ns("metadata_count"), inline = TRUE)
          ),
          shiny::conditionalPanel(
            condition = "input.metadata_view === 'table'",
            shiny::uiOutput(ns("metadata_pagination")),
            ns = ns
          )
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
      ),
      bslib::nav_panel(
        "Datasets",
        shiny::tags$p(
          class = "text-body-secondary",
          shiny::textOutput(ns("dataset_count"), inline = TRUE)
        ),
        shiny::tableOutput(ns("dataset_table"))
      ),
      bslib::nav_panel(
        "Variables",
        shiny::tags$div(
          class = paste(
            "d-flex flex-wrap align-items-center",
            "justify-content-between gap-2 mb-3"
          ),
          shiny::tags$p(
            class = "text-body-secondary mb-0",
            shiny::textOutput(ns("variable_count"), inline = TRUE)
          ),
          shiny::uiOutput(ns("variable_pagination"))
        ),
        shiny::tableOutput(ns("variable_table"))
      ),
      bslib::nav_panel(
        "File groups",
        shiny::tags$div(
          class = paste(
            "d-flex flex-wrap align-items-center",
            "justify-content-between gap-2 mb-3"
          ),
          shiny::tags$p(
            class = "text-body-secondary mb-0",
            shiny::textOutput(ns("file_group_count"), inline = TRUE)
          ),
          shiny::uiOutput(ns("file_group_pagination"))
        ),
        shiny::tableOutput(ns("file_group_table"))
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
  select_nav = bslib::nav_select,
  show_modal = shiny::showModal
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
    handoff_request <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(glc_explorer_status(
      "Open a package before exploring its contents.",
      "empty"
    ))
    loaded_key <- NULL
    observed_key <- NULL
    request_id <- 0L
    handoff_request_id <- 0L
    metadata_batch_size <- 25L
    metadata_limits <- shiny::reactiveValues()
    file_group_page_number <- shiny::reactiveVal(1L)
    variable_page_number <- shiny::reactiveVal(1L)
    registered_metadata_resources <- character()
    registered_metadata_entries <- character()
    file_group_filter_ids <- paste0(
      "file_group_",
      c(
        "device_id",
        "device_location",
        "location_type",
        "modality",
        "role",
        "state",
        "variable",
        "term"
      )
    )

    clear_file_group_filters <- function() {
      for (input_id in file_group_filter_ids) {
        shiny::updateSelectizeInput(
          session,
          input_id,
          selected = character()
        )
      }
    }

    update_file_group_filter_choices <- function(file_groups) {
      choices <- glc_explorer_file_group_field_choices(file_groups)
      for (field in names(choices)) {
        values <- choices[[field]]
        shiny::updateSelectizeInput(
          session,
          paste0("file_group_", field),
          choices = stats::setNames(values, values),
          selected = character(),
          server = TRUE
        )
      }
    }

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

      result$file_groups <- glc_explorer_file_group_inventory(
        result$files,
        result$variables
      )
      contents(result)
      loaded_key <<- key
      status(glc_explorer_status(
        sprintf(
          paste0(
            "Loaded %d datasets, %d file groups, %d variables, ",
            "and %d metadata values."
          ),
          nrow(result$datasets),
          nrow(result$file_groups),
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
        handoff_request(NULL)
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
      status(glc_explorer_status(
        paste(
          "Loading package contents:",
          "datasets, file groups, variables, and metadata\u2026"
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
          selected_datasets <- glc_explorer_file_group_navigation_selection(
            request,
            value$datasets %||% NULL
          )
          shiny::updateSelectizeInput(
            session,
            "file_group_dataset_id",
            selected = selected_datasets
          )
          shiny::updateTextInput(
            session,
            "file_group_query",
            value = ""
          )
          clear_file_group_filters()
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
        update_file_group_filter_choices(
          value$file_groups
        )
        file_group_page_number(1L)
        variable_page_number(1L)
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
      value$file_groups
    })

    filtered_file_groups <- shiny::reactive({
      value <- file_groups()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_filter_file_groups(
        value,
        dataset_ids = input$file_group_dataset_id %||% character(),
        query = input$file_group_query %||% "",
        device_ids = input$file_group_device_id %||% character(),
        device_locations = input$file_group_device_location %||% character(),
        location_types = input$file_group_location_type %||% character(),
        modalities = input$file_group_modality %||% character(),
        roles = input$file_group_role %||% character(),
        states = input$file_group_state %||% character(),
        variable_names = input$file_group_variable %||% character(),
        terms = input$file_group_term %||% character()
      )
    })

    compatibility_groups <- shiny::reactive({
      value <- package()
      if (is.null(value) || is.null(contents())) {
        return(NULL)
      }
      glc_explorer_group_inventory(value)
    })

    file_group_compatibility <- shiny::reactive({
      filtered <- filtered_file_groups()
      groups <- compatibility_groups()
      if (is.null(filtered) || is.null(groups)) {
        return(NULL)
      }
      glc_explorer_file_group_compatibility(
        groups,
        filtered$file_group_id
      )
    })

    shiny::observeEvent(
      input$file_group_handoff,
      {
        compatibility <- shiny::isolate(file_group_compatibility())
        if (
          is.null(compatibility) ||
            identical(compatibility$state %||% "zero", "zero")
        ) {
          return()
        }
        if (!isTRUE(compatibility$ok)) {
          show_modal(glc_explorer_file_group_compatibility_modal(
            compatibility
          ))
          return()
        }
        filtered <- shiny::isolate(filtered_file_groups())
        value <- shiny::isolate(package())
        if (is.null(filtered) || nrow(filtered) == 0L || is.null(value)) {
          return()
        }

        handoff_request_id <<- handoff_request_id + 1L
        handoff_request(list(
          package_key = glc_explorer_package_key(value),
          request_id = handoff_request_id,
          dataset_ids = glc_explorer_nonempty_values(filtered$dataset_id),
          file_group_ids = glc_explorer_nonempty_values(
            filtered$file_group_id
          )
        ))
      },
      ignoreInit = TRUE
    )

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

    file_group_page <- shiny::reactive({
      glc_explorer_inventory_page(
        filtered_file_groups(),
        page = file_group_page_number(),
        page_size = 100L
      )
    })
    variable_page <- shiny::reactive({
      glc_explorer_inventory_page(
        filtered_variables(),
        page = variable_page_number(),
        page_size = 100L
      )
    })

    shiny::observeEvent(
      list(
        input$file_group_dataset_id,
        input$file_group_query,
        input$file_group_device_id,
        input$file_group_device_location,
        input$file_group_location_type,
        input$file_group_modality,
        input$file_group_role,
        input$file_group_state,
        input$file_group_variable,
        input$file_group_term
      ),
      file_group_page_number(1L),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      list(
        input$variable_dataset_id,
        input$variable_query,
        input$primary_only
      ),
      variable_page_number(1L),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$file_group_previous,
      {
        page <- file_group_page()
        if (!is.null(page)) {
          file_group_page_number(max(1L, page$page - 1L))
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$file_group_next,
      {
        page <- file_group_page()
        if (!is.null(page)) {
          file_group_page_number(min(page$page_count, page$page + 1L))
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$variable_previous,
      {
        page <- variable_page()
        if (!is.null(page)) {
          variable_page_number(max(1L, page$page - 1L))
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$variable_next,
      {
        page <- variable_page()
        if (!is.null(page)) {
          variable_page_number(min(page$page_count, page$page + 1L))
        }
      },
      ignoreInit = TRUE
    )

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

    metadata_hierarchy_active <- shiny::reactive({
      identical(input$contents_tab, "Metadata") &&
        identical(input$metadata_view %||% "hierarchy", "hierarchy")
    })
    metadata_table_active <- shiny::reactive({
      identical(input$contents_tab, "Metadata") &&
        identical(input$metadata_view %||% "hierarchy", "table")
    })

    hierarchy_metadata <- shiny::reactive({
      if (!isTRUE(metadata_hierarchy_active())) {
        return(NULL)
      }
      value <- contents()
      matches <- filtered_metadata()
      if (is.null(value) || is.null(matches)) {
        return(NULL)
      }
      glc_explorer_complete_metadata_entries(
        value$metadata,
        matches
      )
    })

    hierarchy_entries <- shiny::reactive({
      glc_explorer_metadata_entry_index(hierarchy_metadata())
    })

    hierarchy_resources <- shiny::reactive({
      glc_explorer_metadata_resource_index(
        hierarchy_metadata(),
        entries = hierarchy_entries()
      )
    })

    visible_hierarchy_entries <- shiny::reactive({
      entries <- hierarchy_entries()
      opened <- input$metadata_resources %||% character()
      if (nrow(entries) == 0L || length(opened) == 0L) {
        return(entries[FALSE, , drop = FALSE])
      }
      loaded_entries <- lapply(opened, function(resource_id) {
        resource_entries <- entries[
          entries$resource_id == resource_id,
          ,
          drop = FALSE
        ]
        if (nrow(resource_entries) == 0L) {
          return(NULL)
        }
        limit <- metadata_limits[[resource_id]] %||% metadata_batch_size
        utils::head(resource_entries, limit)
      })
      dplyr::bind_rows(loaded_entries)
    })

    metadata_filter_key <- shiny::reactive({
      if (!isTRUE(metadata_hierarchy_active())) {
        return(NULL)
      }
      value <- contents()
      paste(
        glc_explorer_package_key(package()) %||% "",
        input$metadata_resource %||% "all",
        input$metadata_query %||% "",
        nrow(value$metadata %||% data.frame()),
        sep = "\r"
      )
    })

    shiny::observeEvent(
      metadata_filter_key(),
      {
        resources <- hierarchy_resources()
        for (resource_id in resources$resource_id) {
          metadata_limits[[resource_id]] <- metadata_batch_size
        }
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    shiny::observe({
      if (!isTRUE(metadata_hierarchy_active())) {
        return()
      }
      resources <- hierarchy_resources()
      for (row in seq_len(nrow(resources))) {
        resource_id <- resources$resource_id[[row]]
        if (resource_id %in% registered_metadata_resources) {
          next
        }
        registered_metadata_resources <<- c(
          registered_metadata_resources,
          resource_id
        )
        local({
          current_resource_id <- resource_id
          body_id <- paste0(
            "metadata_resource_body_",
            current_resource_id
          )
          records_id <- paste0(
            "metadata_records_",
            current_resource_id
          )
          more_id <- paste0("metadata_more_", current_resource_id)

          output[[body_id]] <- shiny::renderUI({
            opened <- input$metadata_resources %||% character()
            if (!current_resource_id %in% opened) {
              return(NULL)
            }
            value <- hierarchy_metadata()
            if (is.null(value)) {
              return(NULL)
            }
            rows <- value[
              value$.resource_id == current_resource_id,
              ,
              drop = FALSE
            ]
            limit <- metadata_limits[[current_resource_id]] %||%
              metadata_batch_size
            slice <- glc_explorer_metadata_entry_slice(rows, limit)
            open_entries <- shiny::isolate(
              input[[records_id]] %||% FALSE
            )
            glc_explorer_metadata_resource_body_tag(
              slice,
              ns = session$ns,
              open_entries = open_entries,
              batch_size = metadata_batch_size
            )
          })

          shiny::observeEvent(
            input[[more_id]],
            {
              value <- hierarchy_metadata()
              if (is.null(value)) {
                return()
              }
              rows <- value[
                value$.resource_id == current_resource_id,
                ,
                drop = FALSE
              ]
              total <- nrow(glc_explorer_metadata_entry_index(rows))
              current <- metadata_limits[[current_resource_id]] %||%
                metadata_batch_size
              metadata_limits[[current_resource_id]] <- min(
                total,
                current + metadata_batch_size
              )
            },
            ignoreInit = TRUE,
            ignoreNULL = TRUE
          )
        })
      }
    })

    shiny::observe({
      if (!isTRUE(metadata_hierarchy_active())) {
        return()
      }
      entries <- visible_hierarchy_entries()
      for (row in seq_len(nrow(entries))) {
        entry_id <- entries$entry_id[[row]]
        if (entry_id %in% registered_metadata_entries) {
          next
        }
        registered_metadata_entries <<- c(
          registered_metadata_entries,
          entry_id
        )
        local({
          current_entry_id <- entry_id
          body_id <- paste0("metadata_entry_body_", current_entry_id)
          output[[body_id]] <- shiny::renderUI({
            value <- hierarchy_metadata()
            if (is.null(value)) {
              return(NULL)
            }
            rows <- value[
              value$.entry_id == current_entry_id,
              ,
              drop = FALSE
            ]
            if (nrow(rows) == 0L) {
              return(NULL)
            }
            records_id <- paste0(
              "metadata_records_",
              rows$.resource_id[[1L]]
            )
            opened <- input[[records_id]] %||% character()
            if (!current_entry_id %in% opened) {
              return(NULL)
            }
            glc_explorer_metadata_entry_body_tag(rows)
          })
        })
      }
    })

    metadata_page <- shiny::reactive({
      if (!isTRUE(metadata_table_active())) {
        return(NULL)
      }
      glc_explorer_metadata_page(
        filtered_metadata(),
        page = input$metadata_page %||% 1L,
        page_size = input$metadata_page_size %||% 500L
      )
    })

    shiny::observeEvent(
      list(
        input$metadata_resource,
        input$metadata_query,
        input$metadata_page_size
      ),
      shiny::updateNumericInput(session, "metadata_page", value = 1L),
      ignoreInit = TRUE
    )
    shiny::observe({
      if (!isTRUE(metadata_table_active())) {
        return()
      }
      page <- metadata_page()
      if (is.null(page)) {
        return()
      }
      current <- suppressWarnings(as.integer(input$metadata_page %||% 1L))
      value <- if (is.na(current) || !identical(current, page$page)) {
        page$page
      } else {
        NULL
      }
      shiny::updateNumericInput(
        session,
        "metadata_page",
        value = value,
        min = 1L,
        max = page$page_count
      )
    })
    shiny::observeEvent(
      input$metadata_previous,
      {
        page <- metadata_page()
        if (!is.null(page)) {
          shiny::updateNumericInput(
            session,
            "metadata_page",
            value = max(1L, page$page - 1L)
          )
        }
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$metadata_next,
      {
        page <- metadata_page()
        if (!is.null(page)) {
          shiny::updateNumericInput(
            session,
            "metadata_page",
            value = min(page$page_count, page$page + 1L)
          )
        }
      },
      ignoreInit = TRUE
    )

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
      glc_explorer_inventory_page_message(
        file_group_page(),
        "file groups"
      )
    })
    output$file_group_compatibility <- shiny::renderUI({
      value <- file_group_compatibility()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_file_group_compatibility_tag(
        value,
        action_id = session$ns("file_group_handoff")
      )
    })
    output$variable_count <- shiny::renderText({
      glc_explorer_inventory_page_message(
        variable_page(),
        "variable declarations"
      )
    })
    output$file_group_pagination <- shiny::renderUI({
      glc_explorer_inventory_pagination_tag(
        file_group_page(),
        input_prefix = "file_group",
        item = "file groups",
        ns = session$ns
      )
    })
    output$variable_pagination <- shiny::renderUI({
      glc_explorer_inventory_pagination_tag(
        variable_page(),
        input_prefix = "variable",
        item = "variables",
        ns = session$ns
      )
    })
    output$metadata_count <- shiny::renderText({
      if (identical(input$metadata_view %||% "hierarchy", "table")) {
        return(glc_explorer_metadata_page_message(metadata_page()))
      }
      glc_explorer_metadata_hierarchy_message(
        filtered_metadata(),
        hierarchy_metadata(),
        entries = hierarchy_entries(),
        resources = hierarchy_resources()
      )
    })
    output$metadata_pagination <- shiny::renderUI({
      if (!identical(input$metadata_view %||% "hierarchy", "table")) {
        return(NULL)
      }
      glc_explorer_metadata_pagination_tag(
        metadata_page(),
        ns = session$ns
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
        page <- file_group_page()
        if (is.null(page)) {
          return(NULL)
        }
        glc_explorer_file_group_table(page$data)
      },
      rownames = FALSE,
      bordered = FALSE,
      spacing = "m"
    )
    output$variable_table <- shiny::renderTable(
      {
        page <- variable_page()
        if (is.null(page)) {
          return(NULL)
        }
        glc_explorer_variable_table(page$data)
      },
      rownames = FALSE,
      bordered = FALSE,
      spacing = "m"
    )
    output$metadata_table <- shiny::renderTable(
      {
        page <- metadata_page()
        if (is.null(page)) {
          return(NULL)
        }
        glc_explorer_metadata_table(page$data)
      },
      rownames = FALSE,
      bordered = FALSE,
      spacing = "m"
    )
    output$metadata_hierarchy <- shiny::renderUI({
      value <- hierarchy_metadata()
      if (is.null(value)) {
        return(NULL)
      }
      resource <- input$metadata_resource %||% "all"
      glc_explorer_metadata_hierarchy_tag(
        value,
        ns = session$ns,
        open_resource = if (identical(resource, "all")) NULL else resource,
        resources = hierarchy_resources()
      )
    })

    list(
      contents = shiny::reactive(contents()),
      datasets = filtered_datasets,
      file_groups = filtered_file_groups,
      variables = filtered_variables,
      compatibility_groups = compatibility_groups,
      file_group_compatibility = file_group_compatibility,
      handoff_request = shiny::reactive(handoff_request()),
      file_group_page = file_group_page,
      variable_page = variable_page,
      metadata = filtered_metadata,
      metadata_page = metadata_page,
      hierarchy_metadata = hierarchy_metadata,
      hierarchy_entries = hierarchy_entries,
      hierarchy_resources = hierarchy_resources,
      visible_hierarchy_entries = visible_hierarchy_entries,
      metadata_hierarchy_active = metadata_hierarchy_active,
      metadata_table_active = metadata_table_active,
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
