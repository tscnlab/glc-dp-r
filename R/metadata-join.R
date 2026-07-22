glc_metadata_fields <- function(fields) {
  if (
    !is.character(fields) ||
      length(fields) == 0L ||
      anyNA(fields) ||
      any(!nzchar(fields))
  ) {
    glc_abort(
      "{.arg fields} must contain one or more non-empty column names.",
      class = "glcdp_metadata_field_error"
    )
  }
  unique(fields)
}

glc_metadata_join_keys <- function(by) {
  if (
    !is.character(by) ||
      length(by) != 1L ||
      is.na(by) ||
      !nzchar(by)
  ) {
    glc_abort(
      "{.arg by} must be one non-empty column name or one named column mapping.",
      class = "glcdp_metadata_by_error"
    )
  }

  by_names <- names(by)
  data_key <- if (is.null(by_names)) by[[1L]] else by_names[[1L]]
  if (is.na(data_key) || !nzchar(data_key)) {
    glc_abort(
      "A named {.arg by} mapping must name the dataset column.",
      class = "glcdp_metadata_by_error"
    )
  }

  list(data = data_key, metadata = unname(by[[1L]]))
}

glc_validate_metadata_table <- function(metadata, source) {
  if (!is.data.frame(metadata)) {
    glc_abort(
      "Metadata source {source} does not resolve to one tabular resource.",
      class = "glcdp_metadata_source_error"
    )
  }
  if (is.null(names(metadata)) || any(!nzchar(names(metadata)))) {
    glc_abort(
      "Metadata source {source} must have non-empty column names.",
      class = "glcdp_metadata_source_error"
    )
  }
  if (anyDuplicated(names(metadata))) {
    duplicated_names <- unique(names(metadata)[duplicated(names(metadata))])
    glc_abort(
      "Metadata source {source} has duplicate column name{?s}: {.val {duplicated_names}}.",
      class = "glcdp_metadata_source_error"
    )
  }
  tibble::as_tibble(metadata, .name_repair = "minimal")
}

glc_read_metadata_path <- function(path) {
  glc_assert_string(path, "metadata")
  if (!file.exists(path) || dir.exists(path)) {
    glc_abort(
      "Metadata file does not exist: {.path {path}}.",
      class = "glcdp_metadata_source_error"
    )
  }

  extension <- tolower(tools::file_ext(path))
  if (!extension %in% c("csv", "tsv")) {
    glc_abort(
      "Metadata file {.path {path}} must use the `.csv` or `.tsv` extension.",
      class = "glcdp_metadata_source_error"
    )
  }
  delimiter <- if (identical(extension, "tsv")) "\t" else ","

  tryCatch(
    readr::read_delim(
      path,
      delim = delimiter,
      show_col_types = FALSE,
      progress = FALSE,
      name_repair = "minimal"
    ),
    error = function(cnd) {
      glc_abort(
        "Could not read metadata file {.path {path}}: {conditionMessage(cnd)}",
        class = "glcdp_metadata_source_error",
        parent = cnd
      )
    }
  )
}

glc_metadata_record_table <- function(value, source) {
  if (is.data.frame(value)) {
    return(glc_validate_metadata_table(value, source))
  }
  if (!is.list(value)) {
    glc_abort(
      "Metadata source {source} does not contain tabular records.",
      class = "glcdp_metadata_source_error"
    )
  }

  records <- glc_records(value)
  if (length(records) == 0L) {
    return(tibble::tibble())
  }
  rows <- lapply(records, function(record) {
    if (
      !is.list(record) ||
        is.null(names(record)) ||
        any(!nzchar(names(record)))
    ) {
      glc_abort(
        "Metadata source {source} does not contain named records.",
        class = "glcdp_metadata_source_error"
      )
    }
    if (anyDuplicated(names(record))) {
      duplicated_names <- unique(names(record)[duplicated(names(record))])
      glc_abort(
        "Metadata source {source} has duplicate column name{?s}: {.val {duplicated_names}}.",
        class = "glcdp_metadata_source_error"
      )
    }
    cells <- lapply(record, function(item) {
      if (is.atomic(item) && length(item) == 1L) item else list(item)
    })
    tibble::as_tibble_row(cells)
  })
  glc_validate_metadata_table(dplyr::bind_rows(rows), source)
}

glc_metadata_package_table <- function(package, resource) {
  descriptor <- glc_resource_descriptor(package, resource)
  paths <- glc_compact_character(descriptor$path)
  if (length(paths) == 0L || any(endsWith(paths, "/"))) {
    glc_abort(
      "Metadata resource {.val {resource}} does not resolve to one set of tabular records.",
      class = "glcdp_metadata_source_error"
    )
  }

  value <- glc_metadata(package, resources = resource)[[resource]]
  source <- paste0("`", resource, "`")
  if (length(paths) == 1L) {
    return(glc_metadata_record_table(value, source))
  }
  if (!is.list(value) || length(value) != length(paths)) {
    glc_abort(
      "Metadata resource {.val {resource}} could not be combined across its declared files.",
      class = "glcdp_metadata_source_error"
    )
  }
  tables <- lapply(value, glc_metadata_record_table, source = source)
  glc_validate_metadata_table(dplyr::bind_rows(tables), source)
}

glc_metadata_package_tables <- function(package, resource = NULL) {
  resources <- glc_resources(package)
  resource_names <- unique(resources$resource[resources$directory %in% FALSE])
  if (!is.null(resource)) {
    glc_assert_string(resource, "resource")
    if (!resource %in% unique(resources$resource)) {
      glc_abort(
        "Data package does not declare a resource named {.val {resource}}.",
        class = "glcdp_missing_resource"
      )
    }
    resource_names <- resource
  }

  tables <- list()
  failed <- character()
  for (name in resource_names) {
    value <- tryCatch(
      glc_metadata_package_table(package, name),
      error = identity
    )
    if (inherits(value, "error")) {
      if (!is.null(resource)) {
        stop(value)
      }
      failed <- c(failed, name)
    } else {
      tables[[name]] <- value
    }
  }
  list(tables = tables, failed = failed)
}

glc_infer_metadata_resource <- function(package, fields, metadata_key) {
  inspected <- glc_metadata_package_tables(package)
  tables <- inspected$tables
  failed <- inspected$failed

  candidates <- names(tables)[vapply(
    tables,
    function(table) {
      metadata_key %in% names(table) && any(fields %in% names(table))
    },
    logical(1)
  )]

  if (length(candidates) == 1L && length(failed) == 0L) {
    return(tables[[candidates]])
  }
  if (length(candidates) == 1L && length(failed) > 0L) {
    glc_abort(
      c(
        "Metadata resource detection is incomplete.",
        "i" = "Matching resource: {.val {candidates}}.",
        "!" = "Resources that could not be inspected: {.val {failed}}.",
        "i" = "Supply {.arg resource} explicitly."
      ),
      class = "glcdp_metadata_source_error"
    )
  }
  if (length(candidates) > 1L) {
    glc_abort(
      c(
        "Metadata resource detection is ambiguous.",
        "i" = "Resources matching the join key and requested fields: {.val {candidates}}.",
        "i" = "Supply {.arg resource} explicitly."
      ),
      class = "glcdp_metadata_resource_ambiguous"
    )
  }

  key_resources <- names(tables)[vapply(
    tables,
    function(table) metadata_key %in% names(table),
    logical(1)
  )]
  if (length(key_resources) > 0L) {
    glc_abort(
      c(
        "None of the requested metadata fields were found.",
        "i" = "Requested fields: {.val {fields}}.",
        "i" = "Resources containing the metadata join key: {.val {key_resources}}."
      ),
      class = "glcdp_metadata_field_no_match"
    )
  }

  details <- if (length(failed) > 0L) {
    c("i" = "Resources that could not be inspected: {.val {failed}}.")
  } else {
    character()
  }
  glc_abort(
    c(
      "No declared tabular metadata resource contains the join column {.val {metadata_key}} and a requested field.",
      details,
      "i" = "Supply {.arg resource} explicitly or pass a metadata table."
    ),
    class = "glcdp_metadata_source_error"
  )
}

glc_resolve_metadata_table <- function(
  metadata,
  fields,
  metadata_key,
  resource
) {
  if (!is.null(resource)) {
    glc_assert_string(resource, "resource")
  }

  if (inherits(metadata, "glc_package")) {
    if (is.null(resource)) {
      return(glc_infer_metadata_resource(metadata, fields, metadata_key))
    }
    return(glc_metadata_package_table(metadata, resource))
  }

  if (!is.null(resource)) {
    glc_abort(
      "{.arg resource} can only be used when {.arg metadata} is a package opened with {.fn glc_open}.",
      class = "glcdp_metadata_source_error"
    )
  }
  if (is.data.frame(metadata)) {
    return(glc_validate_metadata_table(metadata, "the supplied table"))
  }
  if (is.character(metadata) && length(metadata) == 1L && !is.na(metadata)) {
    table <- glc_read_metadata_path(metadata)
    return(glc_validate_metadata_table(table, "the supplied file"))
  }

  glc_abort(
    "{.arg metadata} must be a data frame, a local CSV/TSV path, or a package opened with {.fn glc_open}.",
    class = "glcdp_metadata_source_error"
  )
}

glc_metadata_identifier_values <- function(value, source) {
  if (is.data.frame(value) || is.list(value) || !is.null(dim(value))) {
    glc_abort(
      "The identifier column in {source} must be an atomic vector.",
      class = "glcdp_metadata_id_error"
    )
  }

  identifiers <- tryCatch(
    as.character(value),
    error = function(cnd) {
      glc_abort(
        "The identifier column in {source} cannot be converted to character.",
        class = "glcdp_metadata_id_error",
        parent = cnd
      )
    }
  )
  invalid <- is.na(identifiers) | !nzchar(trimws(identifiers))
  if (any(invalid)) {
    glc_abort(
      "The identifier column in {source} contains missing or empty values.",
      class = "glcdp_metadata_id_error"
    )
  }
  identifiers
}

glc_metadata_matched_column <- function(value, index) {
  if (is.data.frame(value)) {
    return(value[index, , drop = FALSE])
  }
  if (is.list(value)) {
    result <- rep(list(NA), length(index))
    matched <- !is.na(index)
    result[matched] <- value[index[matched]]
    return(result)
  }
  value[index]
}

glc_metadata_anchor_level <- function(keys) {
  if (keys$metadata %in% c("file_group_id", ".glc_file_group")) {
    return("file_group")
  }
  if (
    keys$metadata %in%
      c("Id", "dataset_id", "dataset_internal_id", ".glc_dataset_id")
  ) {
    return("dataset")
  }
  NULL
}

glc_metadata_resource_link <- function(
  table,
  resource,
  metadata_key,
  anchor_level
) {
  canonical <- list(
    file_group = unique(c(
      if (identical(anchor_level, "file_group")) metadata_key else character(),
      "file_group_id",
      ".glc_file_group"
    )),
    dataset = unique(c(
      if (identical(anchor_level, "dataset")) {
        setdiff(metadata_key, "Id")
      } else {
        character()
      },
      "dataset_internal_id",
      "dataset_id",
      ".glc_dataset_id"
    )),
    participant = c("participant_internal_id", ".glc_participant_id"),
    study = "study_internal_id",
    device = "device_internal_id"
  )
  canonical <- lapply(canonical, intersect, y = names(table))
  if (length(canonical[[anchor_level]]) > 0L) {
    return(list(
      level = anchor_level,
      key = canonical[[anchor_level]][[1L]]
    ))
  }
  if (length(canonical$dataset) > 0L) {
    return(list(level = "dataset", key = canonical$dataset[[1L]]))
  }
  levels <- names(canonical)[lengths(canonical) > 0L]
  if (length(levels) == 1L) {
    level <- levels[[1L]]
    return(list(level = level, key = canonical[[level]][[1L]]))
  }

  generic <- list(
    file_group = unique(c(
      if (identical(anchor_level, "file_group")) metadata_key else character(),
      "file_group_id"
    )),
    dataset = unique(c(
      if (identical(anchor_level, "dataset")) metadata_key else character(),
      "Id"
    )),
    participant = c("participant_id", "participant_Id"),
    study = "study_id",
    device = "device_id"
  )
  generic <- lapply(generic, intersect, y = names(table))
  if (length(generic[[anchor_level]]) > 0L) {
    return(list(
      level = anchor_level,
      key = generic[[anchor_level]][[1L]]
    ))
  }
  if (length(generic$dataset) > 0L) {
    return(list(level = "dataset", key = generic$dataset[[1L]]))
  }
  if (length(levels) == 0L) {
    levels <- names(generic)[lengths(generic) > 0L]
  }
  if (length(levels) > 1L) {
    columns <- c(
      unlist(canonical[levels], use.names = FALSE),
      unlist(generic[levels], use.names = FALSE)
    )
    glc_abort(
      c(
        "Metadata relationship for resource {.val {resource}} is ambiguous.",
        "!" = "It contains identifiers for multiple levels: {.val {columns}}.",
        "i" = "Use a resource with one relationship key or provide an explicit non-dataset {.arg by} mapping."
      ),
      class = "glcdp_metadata_relationship_ambiguous"
    )
  }
  if (length(levels) == 0L) {
    return(NULL)
  }
  level <- levels[[1L]]
  list(level = level, key = generic[[level]][[1L]])
}

glc_metadata_package_plan <- function(
  package,
  fields,
  metadata_key,
  resource,
  anchor_level
) {
  inspected <- glc_metadata_package_tables(package, resource)
  if (length(inspected$failed) > 0L) {
    glc_abort(
      c(
        "Metadata resource detection is incomplete.",
        "!" = "Resources that could not be inspected: {.val {inspected$failed}}.",
        "i" = "Supply {.arg resource} explicitly."
      ),
      class = "glcdp_metadata_source_error"
    )
  }

  assignments <- stats::setNames(rep(list(character()), length(fields)), fields)
  links <- list()
  for (name in names(inspected$tables)) {
    table <- inspected$tables[[name]]
    available <- intersect(fields, names(table))
    if (length(available) == 0L) {
      next
    }
    link <- glc_metadata_resource_link(
      table,
      name,
      metadata_key,
      anchor_level
    )
    if (is.null(link)) {
      next
    }
    reachable <- if (identical(anchor_level, "file_group")) {
      c("file_group", "dataset", "participant", "study", "device")
    } else {
      c("dataset", "participant", "study", "device")
    }
    if (!link$level %in% reachable) {
      if (!is.null(resource)) {
        glc_abort(
          c(
            "Metadata resource {.val {name}} is not uniquely connected to the selected {.arg by} key.",
            "i" = "Use {.arg by} = {.val file_group_id} for file-group metadata."
          ),
          class = "glcdp_metadata_relationship_ambiguous"
        )
      }
      next
    }
    links[[name]] <- link
    for (field in available) {
      assignments[[field]] <- c(assignments[[field]], name)
    }
  }

  ambiguous <- names(assignments)[lengths(assignments) > 1L]
  if (length(ambiguous) > 0L) {
    details <- vapply(
      ambiguous,
      function(field) {
        paste0(field, " (", paste(assignments[[field]], collapse = ", "), ")")
      },
      character(1)
    )
    glc_abort(
      c(
        "Metadata field detection is ambiguous.",
        "!" = "Fields found in multiple connected resources: {.val {details}}.",
        "i" = "Supply {.arg resource} explicitly."
      ),
      class = "glcdp_metadata_resource_ambiguous"
    )
  }

  found_fields <- fields[lengths(assignments) == 1L]
  if (length(found_fields) == 0L) {
    glc_abort(
      c(
        "None of the requested metadata fields were found in a resource connected to the input identifiers.",
        "i" = "Requested fields: {.val {fields}}."
      ),
      class = "glcdp_metadata_field_no_match"
    )
  }
  resource_names <- unique(unlist(assignments[found_fields], use.names = FALSE))
  list(
    fields = found_fields,
    assignments = assignments[found_fields],
    links = links[resource_names],
    tables = inspected$tables[resource_names]
  )
}

glc_metadata_file_group_inventory <- function(package) {
  datasets <- glc_model(package)$datasets
  rows <- list()
  row_index <- 0L
  for (dataset in datasets) {
    for (group in dataset$groups) {
      row_index <- row_index + 1L
      rows[[row_index]] <- tibble::tibble(
        file_group_id = group$id,
        dataset_id = dataset$id,
        study_id = dataset$study_id,
        participant_id = dataset$participant_id,
        device_id = group$device_id
      )
    }
  }
  if (length(rows) == 0L) {
    return(tibble::tibble(
      file_group_id = character(),
      dataset_id = character(),
      study_id = character(),
      participant_id = character(),
      device_id = character()
    ))
  }
  inventory <- dplyr::bind_rows(rows)
  file_group_ids <- glc_metadata_identifier_values(
    inventory$file_group_id,
    "the package file-group inventory"
  )
  duplicated_ids <- unique(file_group_ids[duplicated(file_group_ids)])
  if (length(duplicated_ids) > 0L) {
    glc_abort(
      c(
        "File-group relationships in the package are contradictory.",
        "!" = "Duplicated file-group identifier{?s}: {.val {duplicated_ids}}."
      ),
      class = "glcdp_metadata_relationship_ambiguous"
    )
  }
  inventory
}

glc_metadata_anchor_inventory <- function(package, anchor_level) {
  if (identical(anchor_level, "file_group")) {
    return(list(
      data = glc_metadata_file_group_inventory(package),
      key = "file_group_id",
      label = "file-group"
    ))
  }
  list(
    data = glc_datasets(package),
    key = "dataset_id",
    label = "dataset"
  )
}

glc_metadata_linked_ids <- function(
  inventory,
  inventory_index,
  level,
  input_ids
) {
  target <- rep(NA_character_, length(input_ids))
  matched <- !is.na(inventory_index)
  if (!any(matched)) {
    return(target)
  }
  rows <- inventory_index[matched]
  if (identical(level, "file_group")) {
    target[matched] <- as.character(inventory$file_group_id[rows])
  } else if (identical(level, "dataset")) {
    target[matched] <- as.character(inventory$dataset_id[rows])
  } else if (identical(level, "participant")) {
    target[matched] <- as.character(inventory$participant_id[rows])
  } else if (identical(level, "study")) {
    target[matched] <- as.character(inventory$study_id[rows])
  } else if (identical(level, "device")) {
    if ("device_id" %in% names(inventory)) {
      target[matched] <- as.character(inventory$device_id[rows])
      return(target)
    }
    device_ids <- lapply(inventory$device_ids[rows], glc_compact_character)
    ambiguous <- lengths(device_ids) > 1L
    if (any(ambiguous)) {
      ambiguous_ids <- input_ids[which(matched)[ambiguous]]
      glc_abort(
        c(
          "Device metadata cannot be resolved unambiguously for every dataset.",
          "!" = "Dataset{?s} linked to multiple devices: {.val {ambiguous_ids}}.",
          "i" = "Use {.arg by} = {.val file_group_id} to resolve device metadata per file group."
        ),
        class = "glcdp_metadata_relationship_ambiguous"
      )
    }
    target[matched] <- vapply(
      device_ids,
      function(ids) if (length(ids) == 1L) ids[[1L]] else NA_character_,
      character(1)
    )
  }
  target
}

glc_warn_missing_metadata_fields <- function(found_fields, fields) {
  missing_fields <- setdiff(fields, found_fields)
  if (length(missing_fields) == 0L) {
    return(invisible(NULL))
  }
  glc_warn(
    c(
      paste0(
        "Only ",
        length(found_fields),
        " of ",
        length(fields),
        " requested metadata fields were found."
      ),
      "!" = "Missing field{?s}: {.val {missing_fields}}."
    ),
    class = "glcdp_metadata_field_partial_match"
  )
}

glc_warn_partial_metadata_ids <- function(matched, input_ids) {
  if (all(matched)) {
    return(invisible(NULL))
  }
  missing_ids <- input_ids[!matched]
  glc_warn(
    c(
      paste0(
        "Metadata fully resolves ",
        sum(matched),
        " of ",
        length(input_ids),
        " input identifiers."
      ),
      "!" = "Unmatched or incompletely matched identifier{?s}: {.val {missing_ids}}."
    ),
    class = "glcdp_metadata_id_partial_match"
  )
}

glc_extract_package_metadata <- function(
  dataset,
  input_ids,
  fields,
  keys,
  package,
  resource,
  anchor_level
) {
  plan <- glc_metadata_package_plan(
    package,
    fields,
    keys$metadata,
    resource,
    anchor_level
  )
  first <- !duplicated(input_ids)
  unique_input_ids <- input_ids[first]
  anchor <- glc_metadata_anchor_inventory(package, anchor_level)
  inventory <- anchor$data
  inventory_index <- match(
    unique_input_ids,
    as.character(inventory[[anchor$key]])
  )
  anchor_matched <- !is.na(inventory_index)
  if (!any(anchor_matched)) {
    glc_abort(
      c(
        "No input identifiers match the package metadata.",
        "i" = "Input key: {.val {keys$data}}; expected {anchor$label} identifiers."
      ),
      class = "glcdp_metadata_id_no_match"
    )
  }

  matches <- list()
  linked <- list()
  metadata_index <- list()
  for (name in names(plan$tables)) {
    table <- plan$tables[[name]]
    link <- plan$links[[name]]
    metadata_ids <- glc_metadata_identifier_values(
      table[[link$key]],
      paste0("metadata resource `", name, "`")
    )
    duplicated_ids <- unique(metadata_ids[duplicated(metadata_ids)])
    if (length(duplicated_ids) > 0L) {
      glc_abort(
        "Metadata identifiers in resource {.val {name}} must be unique; duplicated: {.val {duplicated_ids}}.",
        class = "glcdp_metadata_id_error"
      )
    }
    linked_ids <- glc_metadata_linked_ids(
      inventory,
      inventory_index,
      link$level,
      unique_input_ids
    )
    metadata_index[[name]] <- match(linked_ids, metadata_ids)
    matches[[name]] <- !is.na(metadata_index[[name]])
    linked[[name]] <- anchor_matched & !is.na(linked_ids)
  }

  any_match <- Reduce(`|`, matches)
  any_linked <- Reduce(`|`, linked)
  complete_match <- anchor_matched & Reduce(`&`, matches)
  if (!any(any_match) && any(any_linked)) {
    glc_abort(
      c(
        "No input identifiers match the connected metadata records.",
        "i" = "Input key: {.val {keys$data}}."
      ),
      class = "glcdp_metadata_id_no_match"
    )
  }
  glc_warn_missing_metadata_fields(plan$fields, fields)
  glc_warn_partial_metadata_ids(complete_match, unique_input_ids)

  result <- tibble::tibble(dataset[[keys$data]][first])
  names(result) <- keys$data
  output_fields <- setdiff(plan$fields, keys$data)
  for (field in output_fields) {
    name <- plan$assignments[[field]][[1L]]
    result[[field]] <- glc_metadata_matched_column(
      plan$tables[[name]][[field]],
      metadata_index[[name]]
    )
  }
  result
}

glc_metadata_restore_groups <- function(data, groups, drop) {
  if (length(groups) == 0L) {
    return(data)
  }
  dplyr::group_by_at(data, groups, .drop = drop)
}

glc_metadata_add_group_context <- function(result, dataset, keys) {
  groups <- dplyr::group_vars(dataset)
  if (length(groups) == 0L) {
    return(result)
  }

  metadata_fields <- setdiff(names(result), keys$data)
  collisions <- intersect(groups, metadata_fields)
  if (length(collisions) > 0L) {
    glc_abort(
      c(
        "Requested metadata fields conflict with dataset grouping columns: {.val {collisions}}.",
        "i" = "Ungroup the dataset or rename the metadata fields before extraction."
      ),
      class = "glcdp_metadata_column_conflict"
    )
  }

  context_columns <- unique(c(groups, keys$data))
  working <- dplyr::ungroup(dataset)
  context <- unique(working[, context_columns, drop = FALSE])
  context_ids <- as.character(context[[keys$data]])
  ambiguous_ids <- unique(context_ids[duplicated(context_ids)])
  if (length(ambiguous_ids) > 0L) {
    glc_abort(
      c(
        "Dataset grouping does not map uniquely to the extraction key.",
        "!" = "Identifier{?s} with multiple grouping combinations: {.val {ambiguous_ids}}.",
        "i" = "Each value of {.val {keys$data}} must belong to exactly one combination of the dataset grouping columns."
      ),
      class = "glcdp_metadata_grouping_error"
    )
  }

  context_index <- match(
    as.character(result[[keys$data]]),
    context_ids
  )
  output <- tibble::as_tibble(
    context[context_index, context_columns, drop = FALSE]
  )
  for (field in metadata_fields) {
    output[[field]] <- result[[field]]
  }

  drop <- attr(dplyr::group_data(dataset), ".drop")
  glc_metadata_restore_groups(output, groups, drop)
}

#' Extract metadata for imported data
#'
#' Selects requested metadata fields for the identifiers represented in an
#' imported dataset. By default, the result contains one row per unique file
#' group and can be joined back with [add_metadata()].
#'
#' @param dataset A data frame containing imported observations.
#' @param metadata A metadata data frame, a local CSV or TSV path, or a package
#'   opened with [glc_open()].
#' @param fields One or more exact, top-level metadata column names to select.
#' @param by One common identifier column, or a named character mapping from
#'   the dataset column to the metadata column. The default is
#'   `"file_group_id"`. Use `"Id"` for explicitly dataset-level extraction, or
#'   `c(Id = "dataset_internal_id")` for a differently named metadata key.
#' @param resource An optional declared resource name when `metadata` is a
#'   `glc_package`. For file-group or dataset identifiers, omitting `resource`
#'   searches declared resources connected through the package's file-group,
#'   dataset, participant, study, and device relationships. Each requested field
#'   must resolve to exactly one connected resource. For other `by` mappings,
#'   exactly one declared resource must contain the metadata join column and a
#'   requested field.
#'
#' @details
#' Identifiers are compared as character values, while the identifier column in
#' the returned table retains its original class. Metadata-only identifiers are
#' ignored. If `metadata` is a `glc_package`, the default file-group link may
#' assemble fields from the linked dataset, participant, study, and device
#' records. Dataset-level fields therefore repeat across file groups. Missing
#' participant, study, or device links are retained as missing values with a
#' warning. Use `by = "Id"` for one row per dataset; device fields then error
#' when a dataset is linked to multiple devices.
#'
#' For a grouped dataset, the grouping columns are placed before the extraction
#' key and the original dplyr grouping (including its `.drop` setting) is
#' retained. Each extraction-key value must map to exactly one combination of
#' grouping-column values.
#'
#' If only some input identifiers match, unmatched identifiers are retained with
#' missing metadata and a warning is issued. If only some fields exist, the
#' missing fields are omitted and a warning is issued.
#'
#' Custom metadata are best stored in a declared data-package resource, for
#' example `data/metadata.csv`, rather than discovered from the working
#' directory or neighboring files.
#'
#' @return `extract_metadata()` returns a tibble with one row per unique value
#'   of `by` in first-occurrence order. The dataset's dplyr grouping columns and
#'   grouping are retained, with the `by` column added when it is not already a
#'   grouping column. With the default `by`, this is one row per file group.
#' @export
#'
#' @examples
#' dataset <- tibble::tibble(
#'   Id = c("DS1", "DS1", "DS2"),
#'   file_group_id = c("DS1:1", "DS1:1", "DS2:1"),
#'   value = c(1, 2, 3)
#' ) |>
#'   dplyr::group_by(Id)
#' metadata <- tibble::tibble(
#'   file_group_id = c("DS1:1", "DS2:1"),
#'   condition = c("control", "intervention")
#' )
#'
#' extract_metadata(dataset, metadata, fields = "condition")
#'
#' \dontrun{
#' package <- glc_open("owner/repository")
#' imported <- glc_read(package, dataset_id = "DS1") |>
#'   glc_collect()
#' extract_metadata(
#'   imported,
#'   package,
#'   fields = c("participant_age", "study_title", "device_model")
#' )
#' extract_metadata(imported, package, "dataset_timezone", by = "Id")
#' }
extract_metadata <- function(
  dataset,
  metadata,
  fields,
  by = "file_group_id",
  resource = NULL
) {
  if (!is.data.frame(dataset)) {
    glc_abort(
      "{.arg dataset} must be a data frame.",
      class = "glcdp_metadata_dataset_error"
    )
  }
  fields <- glc_metadata_fields(fields)
  keys <- glc_metadata_join_keys(by)
  if (!keys$data %in% names(dataset)) {
    glc_abort(
      "Dataset join column {.val {keys$data}} is missing.",
      class = "glcdp_metadata_by_error"
    )
  }
  input_ids <- glc_metadata_identifier_values(
    dataset[[keys$data]],
    "the dataset"
  )

  anchor_level <- glc_metadata_anchor_level(keys)
  if (inherits(metadata, "glc_package") && !is.null(anchor_level)) {
    result <- glc_extract_package_metadata(
      dataset,
      input_ids,
      fields,
      keys,
      metadata,
      resource,
      anchor_level
    )
    return(glc_metadata_add_group_context(result, dataset, keys))
  }

  metadata <- glc_resolve_metadata_table(
    metadata,
    fields,
    keys$metadata,
    resource
  )
  if (!keys$metadata %in% names(metadata)) {
    glc_abort(
      "Metadata join column {.val {keys$metadata}} is missing.",
      class = "glcdp_metadata_by_error"
    )
  }

  found_fields <- fields[fields %in% names(metadata)]
  if (length(found_fields) == 0L) {
    glc_abort(
      c(
        "None of the requested metadata fields were found.",
        "i" = "Requested fields: {.val {fields}}."
      ),
      class = "glcdp_metadata_field_no_match"
    )
  }
  metadata_ids <- glc_metadata_identifier_values(
    metadata[[keys$metadata]],
    "the metadata"
  )
  duplicated_ids <- unique(metadata_ids[duplicated(metadata_ids)])
  if (length(duplicated_ids) > 0L) {
    glc_abort(
      "Metadata identifier{?s} must be unique; duplicated: {.val {duplicated_ids}}.",
      class = "glcdp_metadata_id_error"
    )
  }

  first <- !duplicated(input_ids)
  unique_input_ids <- input_ids[first]
  metadata_index <- match(unique_input_ids, metadata_ids)
  matched <- !is.na(metadata_index)
  if (!any(matched)) {
    glc_abort(
      c(
        "No input identifiers match the metadata.",
        "i" = "Input key: {.val {keys$data}}; metadata key: {.val {keys$metadata}}."
      ),
      class = "glcdp_metadata_id_no_match"
    )
  }
  glc_warn_missing_metadata_fields(found_fields, fields)
  glc_warn_partial_metadata_ids(matched, unique_input_ids)

  result <- tibble::tibble(dataset[[keys$data]][first])
  names(result) <- keys$data
  output_fields <- setdiff(found_fields, keys$data)
  for (field in output_fields) {
    result[[field]] <- glc_metadata_matched_column(
      metadata[[field]],
      metadata_index
    )
  }
  glc_metadata_add_group_context(result, dataset, keys)
}

#' Add metadata to imported data
#'
#' Extracts requested metadata with [extract_metadata()] and joins it onto every
#' matching observation in an imported dataset.
#'
#' @inheritParams extract_metadata
#' @param overwrite Replace existing dataset columns that have the same names
#'   as extracted metadata fields. The default is `FALSE`.
#'
#' @return `add_metadata()` returns the original dataset with the requested
#'   metadata columns added. Row order, row count, and dplyr grouping are
#'   preserved.
#' @export
#'
#' @examples
#' dataset <- tibble::tibble(
#'   file_group_id = c("DS1:1", "DS1:1", "DS2:1"),
#'   value = c(1, 2, 3)
#' )
#' metadata <- tibble::tibble(
#'   file_group_id = c("DS1:1", "DS2:1"),
#'   condition = c("control", "intervention")
#' )
#'
#' add_metadata(dataset, metadata, fields = "condition")
add_metadata <- function(
  dataset,
  metadata,
  fields,
  by = "file_group_id",
  resource = NULL,
  overwrite = FALSE
) {
  glc_assert_flag(overwrite, "overwrite")
  keys <- glc_metadata_join_keys(by)
  groups <- dplyr::group_vars(dataset)
  group_drop <- if (length(groups) > 0L) {
    attr(dplyr::group_data(dataset), ".drop")
  } else {
    TRUE
  }
  extraction_dataset <- if (length(groups) > 0L) {
    dplyr::ungroup(dataset)
  } else {
    dataset
  }
  extracted <- extract_metadata(
    extraction_dataset,
    metadata,
    fields,
    by = by,
    resource = resource
  )
  added_fields <- setdiff(names(extracted), keys$data)
  conflicts <- intersect(added_fields, names(dataset))
  if (length(conflicts) > 0L && !overwrite) {
    conflict_label <- if (length(conflicts) == 1L) {
      "Metadata column already exists"
    } else {
      "Metadata columns already exist"
    }
    overwrite_label <- if (length(conflicts) == 1L) "it" else "them"
    glc_abort(
      c(
        "{conflict_label} in the dataset: {.val {conflicts}}.",
        "i" = "Set {.arg overwrite} to `TRUE` to replace {overwrite_label}."
      ),
      class = "glcdp_metadata_column_conflict"
    )
  }

  working <- if (length(groups) > 0L) dplyr::ungroup(dataset) else dataset
  if (overwrite && length(conflicts) > 0L) {
    working <- working[, !names(working) %in% conflicts, drop = FALSE]
  }

  result <- dplyr::left_join(working, extracted, by = keys$data)
  glc_metadata_restore_groups(result, groups, group_drop)
}
