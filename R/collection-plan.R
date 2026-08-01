glc_collection_plan_schema <- function() {
  "glc-collection-plan"
}

glc_collection_plan_version <- function() {
  "1.0.0"
}

glc_plan_utf8_key <- function(x) {
  vapply(
    enc2utf8(as.character(x)),
    function(value) {
      if (is.na(value)) {
        return("0:")
      }
      bytes <- as.integer(charToRaw(value))
      paste0("1:", paste(sprintf("%02x", bytes), collapse = ""))
    },
    character(1)
  )
}

glc_plan_sort_utf8 <- function(x) {
  if (length(x) < 2L) {
    return(as.character(x))
  }
  x <- as.character(x)
  x[order(glc_plan_utf8_key(x), method = "radix")]
}

glc_plan_length_token <- function(type, value = "") {
  value <- enc2utf8(as.character(value))
  paste0(type, ":", nchar(value, type = "bytes"), ":", value)
}

glc_plan_number_text <- function(x) {
  if (is.nan(x)) {
    return("NaN")
  }
  if (is.infinite(x)) {
    return(if (x > 0) "Inf" else "-Inf")
  }
  format(
    x,
    scientific = TRUE,
    digits = 17L,
    trim = TRUE,
    decimal.mark = "."
  )
}

glc_plan_canonical_tokens <- function(x) {
  if (is.null(x)) {
    return("null:0:")
  }
  if (
    is.environment(x) ||
      is.function(x) ||
      is.language(x) ||
      typeof(x) %in% c("externalptr", "weakref")
  ) {
    glc_abort(
      "Collection-plan metadata contains a live or non-serializable value.",
      class = "glcdp_collection_plan_serialization"
    )
  }
  if (is.data.frame(x)) {
    rows <- lapply(seq_len(nrow(x)), function(index) {
      lapply(x, function(column) column[[index]])
    })
    rows <- lapply(rows, stats::setNames, names(x))
    return(c(
      glc_plan_length_token("data-frame", nrow(x)),
      unlist(lapply(rows, glc_plan_canonical_tokens), use.names = FALSE)
    ))
  }
  if (is.list(x)) {
    object_names <- names(x)
    named <- !is.null(object_names) && all(nzchar(object_names))
    if (named) {
      keys <- glc_plan_utf8_key(object_names)
      order_index <- order(keys, seq_along(keys), method = "radix")
      tokens <- glc_plan_length_token("object", length(x))
      for (index in order_index) {
        tokens <- c(
          tokens,
          glc_plan_length_token("key", object_names[[index]]),
          glc_plan_canonical_tokens(x[[index]])
        )
      }
      return(tokens)
    }
    return(c(
      glc_plan_length_token("array", length(x)),
      unlist(lapply(x, glc_plan_canonical_tokens), use.names = FALSE)
    ))
  }
  if (is.character(x)) {
    tokens <- glc_plan_length_token("character", length(x))
    for (value in x) {
      tokens <- c(
        tokens,
        if (is.na(value)) {
          "character-na:0:"
        } else {
          glc_plan_length_token("character-value", value)
        }
      )
    }
    return(tokens)
  }
  if (is.logical(x)) {
    values <- vapply(
      x,
      function(value) {
        if (is.na(value)) "NA" else if (value) "true" else "false"
      },
      character(1)
    )
    return(c(
      glc_plan_length_token("logical", length(x)),
      vapply(
        values,
        glc_plan_length_token,
        character(1),
        type = "logical-value"
      )
    ))
  }
  if (is.integer(x)) {
    values <- ifelse(is.na(x), "NA", as.character(x))
    return(c(
      glc_plan_length_token("integer", length(x)),
      vapply(
        values,
        glc_plan_length_token,
        character(1),
        type = "integer-value"
      )
    ))
  }
  if (is.numeric(x)) {
    values <- vapply(
      x,
      function(value) {
        if (is.na(value) && !is.nan(value)) "NA" else
          glc_plan_number_text(value)
      },
      character(1)
    )
    return(c(
      glc_plan_length_token("double", length(x)),
      vapply(values, glc_plan_length_token, character(1), type = "double-value")
    ))
  }
  if (is.raw(x)) {
    value <- paste(sprintf("%02x", as.integer(x)), collapse = "")
    return(glc_plan_length_token("raw", value))
  }
  glc_abort(
    "Collection-plan metadata contains unsupported type {.val {typeof(x)}}.",
    class = "glcdp_collection_plan_serialization"
  )
}

glc_plan_canonical_text <- function(x) {
  paste(glc_plan_canonical_tokens(x), collapse = "\n")
}

glc_plan_digest <- function(x) {
  digest::digest(
    glc_plan_canonical_text(x),
    algo = "sha256",
    serialize = FALSE
  )
}

glc_plan_safe_metadata <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (
    is.environment(x) ||
      is.function(x) ||
      is.language(x) ||
      typeof(x) %in% c("externalptr", "weakref")
  ) {
    glc_abort(
      "Declaration extensions contain a live or non-serializable value.",
      class = "glcdp_collection_plan_serialization"
    )
  }
  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(index) {
      glc_plan_safe_metadata(lapply(x, function(column) column[[index]]))
    }))
  }
  if (is.list(x)) {
    object_names <- names(x)
    if (!is.null(object_names) && all(nzchar(object_names))) {
      keys <- glc_plan_utf8_key(object_names)
      x <- x[order(keys, seq_along(keys), method = "radix")]
    }
    return(lapply(x, glc_plan_safe_metadata))
  }
  if (is.atomic(x)) {
    return(x)
  }
  glc_abort(
    "Declaration extensions contain unsupported type {.val {typeof(x)}}.",
    class = "glcdp_collection_plan_serialization"
  )
}

glc_plan_unknown_fields <- function(x, known) {
  if (!is.list(x) || is.null(names(x))) {
    return(list())
  }
  keep <- !names(x) %in% known
  glc_plan_safe_metadata(x[keep]) %||% list()
}

glc_plan_character_argument <- function(
  value,
  argument,
  required = FALSE
) {
  if (is.null(value) || length(value) == 0L) {
    if (required) {
      glc_abort("{.arg {argument}} must contain at least one value.")
    }
    return(character())
  }
  if (
    !is.character(value) ||
      anyNA(value) ||
      any(!nzchar(value))
  ) {
    glc_abort(
      "{.arg {argument}} must be a character vector of non-empty values without missing values."
    )
  }
  if (anyDuplicated(value)) {
    duplicated_values <- unique(value[duplicated(value)])
    glc_abort(
      paste0(
        "{.arg {argument}} must not contain duplicates; repeated ",
        "value{?s}: {.val {duplicated_values}}."
      )
    )
  }
  as.character(value)
}

glc_plan_package_id <- function(x) {
  package_id <- glc_scalar_character(x$registry_row$id)
  if (is.na(package_id)) {
    package_id <- glc_scalar_character(x$descriptor$name)
  }
  if (
    is.na(package_id) &&
      !is.na(glc_scalar_character(x$repo))
  ) {
    package_id <- basename(glc_scalar_character(x$repo))
  }
  package_id
}

glc_plan_verified_provenance <- function(x) {
  commit <- glc_scalar_character(x$commit)
  if (is.na(commit) || !grepl("^[0-9a-fA-F]{40}$", commit)) {
    glc_abort(
      "{.arg x} must identify an exact 40-character source revision.",
      class = "glcdp_collection_plan_revision"
    )
  }
  commit <- tolower(commit)
  source_type <- glc_scalar_character(x$source_type)
  verification <- NULL
  latest_pass_commit <- NA_character_
  registry_generated_at <- NA_character_
  manifest_version <- NA_character_

  if (identical(source_type, "remote")) {
    latest_pass_commit <- glc_scalar_character(
      x$registry_row$latest_pass_commit
    )
    registry_generated_at <- glc_scalar_character(
      x$registry_row$registry_generated_at
    )
    if (
      is.na(latest_pass_commit) ||
        !grepl("^[0-9a-fA-F]{40}$", latest_pass_commit) ||
        !identical(commit, tolower(latest_pass_commit))
    ) {
      glc_abort(
        paste0(
          "{.arg x} must be opened at the registry's exact latest passing ",
          "revision before a collection plan can be created."
        ),
        class = "glcdp_collection_plan_revision"
      )
    }
    verification <- "registry_latest_pass"
  } else if (identical(source_type, "local")) {
    manifest_commit <- glc_scalar_character(x$manifest$commit)
    manifest_verified <- glc_scalar_logical(
      x$manifest$registry_verified,
      FALSE
    )
    manifest_version <- glc_scalar_character(x$manifest$manifest_version)
    if (
      is.na(manifest_commit) ||
        !grepl("^[0-9a-fA-F]{40}$", manifest_commit) ||
        !identical(commit, tolower(manifest_commit)) ||
        !isTRUE(manifest_verified)
    ) {
      glc_abort(
        paste0(
          "A local {.arg x} must have a manifest recording the same exact ",
          "revision and `registry_verified = true`."
        ),
        class = "glcdp_collection_plan_revision"
      )
    }
    latest_pass_commit <- commit
    verification <- "manifest_registry_verification"
  } else {
    glc_abort(
      "{.arg x} must be an opened remote or manifest-backed local package.",
      class = "glcdp_collection_plan_revision"
    )
  }

  package_id <- glc_plan_package_id(x)
  if (is.na(package_id) || !nzchar(package_id)) {
    glc_abort(
      "{.arg x} does not declare a stable package identifier.",
      class = "glcdp_collection_plan_provenance"
    )
  }
  schema_version <- glc_scalar_character(x$schema_version)
  if (is.na(schema_version) || !nzchar(schema_version)) {
    glc_abort(
      "{.arg x} does not declare a package schema version.",
      class = "glcdp_collection_plan_provenance"
    )
  }

  list(
    package_id = package_id,
    repository = glc_scalar_character(x$repo),
    source_type = source_type,
    source_revision = commit,
    package_schema_version = schema_version,
    verification = verification,
    latest_pass_commit = tolower(latest_pass_commit),
    registry_generated_at = registry_generated_at,
    manifest_version = manifest_version
  )
}

glc_plan_manifest_bytes <- function(x, path) {
  entries <- x$manifest$files %||% list()
  if (length(entries) == 0L) {
    return(NA_real_)
  }
  matches <- which(vapply(
    entries,
    function(entry) {
      identical(glc_scalar_character(entry$path), path)
    },
    logical(1)
  ))
  if (length(matches) != 1L) {
    return(NA_real_)
  }
  bytes <- glc_scalar_number(entries[[matches]]$bytes)
  if (is.na(bytes) || !is.finite(bytes) || bytes < 0) {
    return(NA_real_)
  }
  bytes
}

glc_plan_variable_extensions <- function(raw_group, variables) {
  raw_variables <- glc_records(raw_group$dataset_file_variables)
  known_variable <- c(
    "dataset_file_variables_name",
    "dataset_file_variables_labels",
    "dataset_file_variables_description",
    "dataset_file_variables_units",
    "dataset_file_variables_calibration",
    "dataset_file_variables_type",
    "dataset_file_variables_factor_levels",
    "dataset_file_variables_term"
  )
  rows <- list()
  for (index in seq_along(variables)) {
    raw_variable <- raw_variables[[index]] %||% list()
    term <- raw_variable$dataset_file_variables_term %||% list()
    raw_levels <- glc_records(
      raw_variable$dataset_file_variables_factor_levels
    )
    level_extensions <- lapply(raw_levels, function(level) {
      glc_plan_unknown_fields(
        level,
        c("value", "label", "description")
      )
    })
    value <- list(
      name = variables[[index]]$name,
      fields = glc_plan_unknown_fields(raw_variable, known_variable),
      term = glc_plan_unknown_fields(
        term,
        c("variable_term", "variable_name")
      ),
      factor_levels = level_extensions
    )
    has_extension <- length(value$fields) > 0L ||
      length(value$term) > 0L ||
      any(vapply(value$factor_levels, length, integer(1)) > 0L)
    if (has_extension) {
      rows[[length(rows) + 1L]] <- value
    }
  }
  rows
}

glc_plan_group_extension <- function(dataset, group) {
  raw_dataset <- dataset$raw %||% list()
  raw_groups <- glc_records(raw_dataset$dataset_file)
  raw_group <- raw_groups[[group$index]] %||% list()
  known_dataset <- c(
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
  known_group <- c(
    "primary_variables",
    "dataset_file_variables",
    "dataset_file_modality",
    "dataset_file_modality_other",
    "dataset_file_modality_other_type",
    "dataset_file_description",
    "dataset_file_crossref_device_id",
    "dataset_file_device_location",
    "dataset_file_device_location_type",
    "dataset_file_temporal_resolution",
    "dataset_file_instructions",
    "dataset_file_names",
    "dataset_file_format",
    "dataset_file_encoding",
    "dataset_file_timezone",
    "dataset_file_datetime",
    "dataset_file_role",
    "dataset_file_auxiliary",
    "dataset_file_data_state",
    "dataset_file_header_row",
    "dataset_file_preprocessing"
  )
  glc_plan_safe_metadata(list(
    dataset = glc_plan_unknown_fields(raw_dataset, known_dataset),
    declared_variable_terms = raw_dataset$dataset_variable_terms %||% list(),
    group = glc_plan_unknown_fields(raw_group, known_group),
    variables = glc_plan_variable_extensions(raw_group, group$variables)
  ))
}

glc_plan_group_records <- function(x) {
  model <- glc_model(x)
  records <- list()
  for (dataset in model$datasets) {
    for (group in dataset$groups) {
      variable_names <- vapply(
        group$variables,
        function(variable) variable$name,
        character(1)
      )
      invalid_names <- is.na(variable_names) | !nzchar(variable_names)
      if (any(invalid_names)) {
        glc_abort(
          "File group {.val {group$id}} declares a variable without a source name.",
          class = "glcdp_collection_plan_ambiguous_variable"
        )
      }
      if (anyDuplicated(variable_names)) {
        duplicates <- unique(variable_names[duplicated(variable_names)])
        glc_abort(
          "File group {.val {group$id}} declares duplicate source name{?s}: {.val {duplicates}}.",
          class = "glcdp_collection_plan_ambiguous_variable"
        )
      }
      files <- lapply(seq_along(group$files), function(file_index) {
        path <- group$files[[file_index]]
        list(
          declared_path = path,
          encoding = group$encodings[[file_index]],
          declared_bytes = glc_plan_manifest_bytes(x, path)
        )
      })
      records[[length(records) + 1L]] <- list(
        dataset_id = dataset$id,
        dataset_schema_version = dataset$schema_version,
        study_id = dataset$study_id,
        participant_id = dataset$participant_id,
        participant_associated = dataset$participant_associated,
        file_group = group$index,
        file_group_id = group$id,
        description = group$description,
        device_id = group$device_id,
        device_location = group$device_location,
        device_location_type = group$device_location_type,
        format = group$format,
        timezone = group$timezone,
        modalities = group$modality,
        modality_other = group$modality_other,
        modality_other_type = group$modality_other_type,
        role = group$role,
        data_state = group$data_state,
        temporal_type = group$temporal_type,
        temporal_value = group$temporal_value,
        temporal_unit = group$temporal_unit,
        header_row = group$header_row,
        preprocessing = group$preprocessing,
        datetime = group$datetime,
        variables = group$variables,
        files = files,
        file_declarations_known = TRUE,
        extensions = glc_plan_group_extension(dataset, group)
      )
    }
  }
  ids <- vapply(records, function(record) record$file_group_id, character(1))
  if (
    anyNA(ids) ||
      any(!nzchar(ids)) ||
      anyDuplicated(ids)
  ) {
    glc_abort(
      "File-group identifiers must be non-empty and unique.",
      class = "glcdp_collection_plan_relationship"
    )
  }
  if (length(records) > 1L) {
    records <- records[order(glc_plan_utf8_key(ids), method = "radix")]
  }
  records
}

glc_plan_datetime_issue <- function(record) {
  specification <- record$datetime
  source <- glc_scalar_character(specification$source)
  date <- glc_scalar_character(specification$date)
  date_format <- glc_scalar_character(specification$date_format)
  time <- glc_scalar_character(specification$time)
  time_format <- glc_scalar_character(specification$time_format)
  variable_names <- vapply(
    record$variables,
    function(variable) variable$name,
    character(1)
  )
  if (
    is.na(source) ||
      !source %in% c("column", "collection") ||
      is.na(date) ||
      !nzchar(date) ||
      is.na(date_format) ||
      !nzchar(date_format)
  ) {
    return(
      "The datetime declaration must provide a supported source, date, and date format."
    )
  }
  if (identical(source, "column") && !date %in% variable_names) {
    return(paste0(
      "Datetime source column ",
      encodeString(date, quote = "\""),
      " is not declared as a variable."
    ))
  }
  if (!is.na(time) && nzchar(time)) {
    if (is.na(time_format) || !nzchar(time_format)) {
      return("The separate time declaration does not provide a time format.")
    }
    if (identical(source, "column") && !time %in% variable_names) {
      return(paste0(
        "Datetime source column ",
        encodeString(time, quote = "\""),
        " is not declared as a variable."
      ))
    }
  }
  NULL
}

glc_plan_reason <- function(code, message) {
  list(code = code, message = message)
}

glc_plan_selected_variables <- function(record, request) {
  variables <- record$variables
  names <- vapply(variables, function(variable) variable$name, character(1))
  terms <- vapply(variables, function(variable) variable$term, character(1))
  keep <- switch(
    request$variable_scope,
    matched = terms %in% request$terms,
    all = rep(TRUE, length(variables)),
    selected = names %in% request$requested_variables
  )
  variables[keep]
}

glc_plan_declared_compatibility <- function(record) {
  variables <- lapply(record$selected_variables, function(variable) {
    list(
      name = variable$name,
      type = variable$type,
      factor_values = vapply(
        variable$factor_levels,
        function(level) level$value,
        character(1)
      ),
      factor_labels = vapply(
        variable$factor_levels,
        function(level) level$label,
        character(1)
      )
    )
  })
  datetime <- record$datetime
  datetime_signature <- glc_datetime_compatibility_signature(
    datetime$source,
    datetime$date,
    datetime$date_format,
    datetime$time,
    datetime$time_format
  )
  if (identical(datetime$source, "collection")) {
    datetime$date <- "<collection-value>"
    if (!is.na(datetime$time) && nzchar(datetime$time)) {
      datetime$time <- "<collection-value>"
    }
  }
  list(
    variables = variables,
    timezone = record$timezone,
    modalities = record$modalities,
    role = record$role,
    data_state = record$data_state,
    datetime = list(
      signature = datetime_signature,
      source = datetime$source,
      date = datetime$date,
      date_format = datetime$date_format,
      time = datetime$time,
      time_format = datetime$time_format
    ),
    relationship_rule = "consistent_dataset_and_file_group_links",
    device_rule = "at_most_one_nonmissing_device_per_dataset"
  )
}

glc_plan_add_reason <- function(reasons, code, message) {
  c(reasons, list(glc_plan_reason(code, message)))
}

glc_plan_evaluate_group <- function(record, request) {
  reasons <- list()
  if (
    length(request$dataset_id) > 0L &&
      !record$dataset_id %in% request$dataset_id
  ) {
    reasons <- glc_plan_add_reason(
      reasons,
      "scope_dataset",
      "The group is outside the requested dataset restriction."
    )
  }
  if (
    length(request$file_group) > 0L &&
      !record$file_group_id %in% request$file_group
  ) {
    reasons <- glc_plan_add_reason(
      reasons,
      "scope_file_group",
      "The group is outside the requested file-group restriction."
    )
  }
  in_scope <- length(reasons) == 0L
  variable_names <- vapply(
    record$variables,
    function(variable) variable$name,
    character(1)
  )
  variable_terms <- vapply(
    record$variables,
    function(variable) variable$term,
    character(1)
  )

  if (in_scope) {
    reserved_names <- variable_names[startsWith(variable_names, ".glc_")]
    if (length(reserved_names) > 0L) {
      reasons <- glc_plan_add_reason(
        reasons,
        "reserved_provenance_column",
        paste0(
          "The group declares source names reserved for glcdp ",
          "provenance: ",
          paste(glc_plan_sort_utf8(reserved_names), collapse = ", "),
          ". Source names beginning with .glc_ cannot be read by glc_read()."
        )
      )
    }
  }

  if (in_scope && length(request$terms) > 0L) {
    missing_terms <- setdiff(request$terms, unique(variable_terms))
    if (length(missing_terms) > 0L) {
      reasons <- glc_plan_add_reason(
        reasons,
        "term_missing",
        paste0(
          "The group does not declare every requested canonical term: ",
          paste(glc_plan_sort_utf8(missing_terms), collapse = ", "),
          "."
        )
      )
    }
  }
  if (in_scope && identical(request$variable_scope, "selected")) {
    missing_variables <- setdiff(
      request$requested_variables,
      variable_names
    )
    if (length(missing_variables) > 0L) {
      reasons <- glc_plan_add_reason(
        reasons,
        "variable_missing",
        paste0(
          "The group does not declare every selected source variable: ",
          paste(glc_plan_sort_utf8(missing_variables), collapse = ", "),
          "."
        )
      )
    }
  }
  selected_variables <- glc_plan_selected_variables(record, request)
  if (in_scope && length(selected_variables) == 0L) {
    reasons <- glc_plan_add_reason(
      reasons,
      "variable_missing",
      paste0(
        "Variable scope ",
        encodeString(request$variable_scope, quote = "\""),
        " resolves to no declared source variables for this group."
      )
    )
  }
  if (
    in_scope &&
      isTRUE(record$file_declarations_known %||% TRUE) &&
      length(record$files) == 0L
  ) {
    reasons <- glc_plan_add_reason(
      reasons,
      "no_declared_files",
      "The group does not declare any measurement files to read."
    )
  }
  if (in_scope && !record$format %in% c("csv", "txt", "tsv")) {
    reasons <- glc_plan_add_reason(
      reasons,
      "unsupported_format",
      paste0(
        "The declared file format ",
        encodeString(record$format, quote = "\""),
        " is not supported by glc_read()."
      )
    )
  }
  if (
    in_scope &&
      (is.na(record$timezone) || !record$timezone %in% OlsonNames())
  ) {
    reasons <- glc_plan_add_reason(
      reasons,
      "invalid_timezone",
      "The group does not declare a valid IANA time zone."
    )
  }
  datetime_issue <- if (in_scope) {
    glc_plan_datetime_issue(record)
  } else {
    NULL
  }
  if (!is.null(datetime_issue)) {
    reasons <- glc_plan_add_reason(
      reasons,
      "incomplete_datetime",
      datetime_issue
    )
  }

  record$selected_variables <- selected_variables
  if (length(reasons) == 0L) {
    record$status <- "included"
    record$reasons <- list(glc_plan_reason(
      "included",
      "The group matches the request and has a complete supported declaration."
    ))
  } else {
    record$status <- "excluded"
    record$reasons <- reasons
  }
  record$unit_id <- NA_character_
  record
}

glc_plan_device_slots <- function(records, indexes) {
  slots <- stats::setNames(rep(1L, length(indexes)), indexes)
  dataset_ids <- unique(vapply(
    records[indexes],
    function(record) record$dataset_id,
    character(1)
  ))
  dataset_ids <- glc_plan_sort_utf8(dataset_ids)
  for (dataset_id in dataset_ids) {
    dataset_indexes <- indexes[vapply(
      records[indexes],
      function(record) {
        identical(record$dataset_id, dataset_id)
      },
      logical(1)
    )]
    device_ids <- vapply(
      records[dataset_indexes],
      function(record) {
        record$device_id
      },
      character(1)
    )
    present <- !is.na(device_ids) & nzchar(device_ids)
    devices <- glc_plan_sort_utf8(unique(device_ids[present]))
    for (position in seq_along(dataset_indexes)) {
      if (present[[position]]) {
        slots[as.character(dataset_indexes[[position]])] <- match(
          device_ids[[position]],
          devices
        )
      }
    }
  }
  as.integer(slots)
}

glc_plan_unit_id <- function(
  compatibility,
  file_group_ids,
  request,
  provenance
) {
  material <- list(
    plan_schema = glc_collection_plan_schema(),
    plan_version = glc_collection_plan_version(),
    package = list(
      package_id = provenance$package_id,
      repository = provenance$repository,
      source_revision = provenance$source_revision,
      package_schema_version = provenance$package_schema_version
    ),
    request = list(
      terms = glc_plan_sort_utf8(request$terms),
      term_match = "all",
      variable_scope = request$variable_scope,
      requested_variables = glc_plan_sort_utf8(
        request$requested_variables
      ),
      dataset_id = glc_plan_sort_utf8(request$dataset_id),
      file_group = glc_plan_sort_utf8(request$file_group),
      standardize = request$standardize
    ),
    compatibility = compatibility,
    file_group_ids = glc_plan_sort_utf8(file_group_ids)
  )
  paste0("glcu_", glc_plan_digest(material))
}

glc_plan_unit_byte_summary <- function(records) {
  files <- unlist(
    lapply(records, function(record) record$files),
    recursive = FALSE
  )
  if (length(files) == 0L) {
    return(list(
      file_count = 0L,
      declared_bytes = 0,
      known_file_count = 0L,
      unknown_file_count = 0L,
      declared_bytes_complete = TRUE
    ))
  }
  bytes <- vapply(files, function(file) file$declared_bytes, numeric(1))
  known <- !is.na(bytes)
  list(
    file_count = length(files),
    declared_bytes = sum(bytes[known]),
    known_file_count = sum(known),
    unknown_file_count = sum(!known),
    declared_bytes_complete = all(known)
  )
}

glc_declared_collection_engine <- function(records, request, provenance) {
  records <- lapply(
    records,
    glc_plan_evaluate_group,
    request = request
  )
  included <- which(vapply(
    records,
    function(record) {
      identical(record$status, "included")
    },
    logical(1)
  ))
  units <- list()

  if (length(included) > 0L) {
    compatibility <- lapply(
      records[included],
      glc_plan_declared_compatibility
    )
    compatibility_text <- vapply(
      compatibility,
      glc_plan_canonical_text,
      character(1)
    )
    bucket_values <- unique(compatibility_text)
    for (bucket_value in bucket_values) {
      bucket_positions <- which(compatibility_text == bucket_value)
      bucket_indexes <- included[bucket_positions]
      slots <- glc_plan_device_slots(records, bucket_indexes)
      for (slot in sort(unique(slots))) {
        member_indexes <- bucket_indexes[slots == slot]
        member_ids <- vapply(
          records[member_indexes],
          function(record) {
            record$file_group_id
          },
          character(1)
        )
        member_ids <- glc_plan_sort_utf8(member_ids)
        contract <- glc_plan_declared_compatibility(
          records[[member_indexes[[1L]]]]
        )
        unit_id <- glc_plan_unit_id(
          contract,
          member_ids,
          request,
          provenance
        )
        for (member_index in member_indexes) {
          records[[member_index]]$unit_id <- unit_id
        }
        member_records <- records[member_indexes]
        byte_summary <- glc_plan_unit_byte_summary(member_records)
        units[[length(units) + 1L]] <- c(
          list(
            unit_id = unit_id,
            file_group_ids = member_ids,
            compatibility = contract,
            dataset_count = length(unique(vapply(
              member_records,
              function(record) record$dataset_id,
              character(1)
            ))),
            file_group_count = length(member_records),
            variable_count = length(contract$variables)
          ),
          byte_summary
        )
      }
    }
  }

  if (length(units) > 0L) {
    unit_ids <- vapply(units, function(unit) unit$unit_id, character(1))
    if (anyDuplicated(unit_ids)) {
      glc_abort(
        "Collection planning produced duplicate unit identifiers.",
        class = "glcdp_collection_plan_id"
      )
    }
    units <- units[order(glc_plan_utf8_key(unit_ids), method = "radix")]
    preferred_order <- order(
      -vapply(units, function(unit) unit$dataset_count, integer(1)),
      -vapply(units, function(unit) unit$file_group_count, integer(1)),
      glc_plan_utf8_key(vapply(
        units,
        function(unit) unit$unit_id,
        character(1)
      )),
      method = "radix"
    )
    preferred_unit_id <- units[[preferred_order[[1L]]]]$unit_id
    units <- lapply(units, function(unit) {
      unit$preferred <- identical(unit$unit_id, preferred_unit_id)
      unit
    })
  } else {
    preferred_unit_id <- NA_character_
  }

  list(
    records = records,
    units = units,
    preferred_unit_id = preferred_unit_id
  )
}

glc_declared_collection_contract_differences <- function(contracts) {
  if (length(contracts) < 2L) {
    return(character())
  }
  first <- contracts[[1L]]
  differences <- character()
  variable_names <- function(contract) {
    vapply(contract$variables, function(variable) variable$name, character(1))
  }
  columns_match <- vapply(
    contracts[-1L],
    function(contract)
      identical(variable_names(contract), variable_names(first)),
    logical(1)
  )
  if (!all(columns_match)) {
    differences <- c(differences, "columns")
  } else {
    variables_match <- vapply(
      contracts[-1L],
      function(contract) identical(contract$variables, first$variables),
      logical(1)
    )
    if (!all(variables_match)) {
      differences <- c(differences, "types_or_factor_levels")
    }
  }
  dimensions <- c(
    timezone = "timezone",
    modalities = "modalities",
    role = "role",
    data_state = "data_state"
  )
  for (code in names(dimensions)) {
    field <- dimensions[[code]]
    matches <- vapply(
      contracts[-1L],
      function(contract) identical(contract[[field]], first[[field]]),
      logical(1)
    )
    if (!all(matches)) {
      differences <- c(differences, code)
    }
  }
  datetime_match <- vapply(
    contracts[-1L],
    function(contract) {
      identical(contract$datetime$signature, first$datetime$signature)
    },
    logical(1)
  )
  if (!all(datetime_match)) {
    differences <- c(differences, "datetime")
  }
  differences
}

glc_declared_collection_unit_differences <- function(units) {
  contracts <- lapply(units, function(unit) unit$compatibility)
  differences <- glc_declared_collection_contract_differences(contracts)
  compatibility_keys <- vapply(contracts, glc_plan_canonical_text, character(1))
  if (anyDuplicated(compatibility_keys)) {
    differences <- c(differences, "device_relationship")
  }
  differences
}

glc_declared_collection_record_differences <- function(records) {
  if (length(records) < 2L) {
    return(character())
  }
  contracts <- lapply(records, glc_plan_declared_compatibility)
  differences <- glc_declared_collection_contract_differences(contracts)
  dataset_ids <- unique(vapply(
    records,
    function(record) record$dataset_id,
    character(1)
  ))
  multiple_devices <- any(vapply(
    dataset_ids,
    function(dataset_id) {
      devices <- vapply(
        records[vapply(
          records,
          function(record) {
            identical(record$dataset_id, dataset_id)
          },
          logical(1)
        )],
        function(record) record$device_id,
        character(1)
      )
      devices <- devices[!is.na(devices) & nzchar(devices)]
      length(unique(devices)) > 1L
    },
    logical(1)
  ))
  if (multiple_devices) {
    differences <- c(differences, "device_relationship")
  }
  differences
}

glc_plan_variable_snapshot <- function(variable) {
  list(
    name = variable$name,
    label = variable$label,
    description = variable$description,
    unit = variable$unit,
    calibration = variable$calibration,
    type = variable$type,
    term = variable$term,
    term_name = variable$term_name,
    primary = variable$primary,
    factor_levels = lapply(variable$factor_levels, function(level) {
      list(
        value = level$value,
        label = level$label,
        description = level$description
      )
    })
  )
}

glc_plan_declaration_snapshot <- function(record) {
  list(
    dataset_id = record$dataset_id,
    dataset_schema_version = record$dataset_schema_version,
    study_id = record$study_id,
    participant_id = record$participant_id,
    participant_associated = record$participant_associated,
    file_group = record$file_group,
    file_group_id = record$file_group_id,
    device_id = record$device_id,
    device_location = record$device_location,
    device_location_type = record$device_location_type,
    format = record$format,
    timezone = record$timezone,
    modalities = record$modalities,
    modality_other = record$modality_other,
    modality_other_type = record$modality_other_type,
    role = record$role,
    data_state = record$data_state,
    temporal_type = record$temporal_type,
    temporal_value = record$temporal_value,
    temporal_unit = record$temporal_unit,
    header_row = record$header_row,
    preprocessing = record$preprocessing,
    datetime = record$datetime,
    variables = lapply(record$variables, glc_plan_variable_snapshot),
    files = record$files,
    extensions = record$extensions
  )
}

glc_plan_empty_units <- function() {
  tibble::tibble(
    unit_id = character(),
    preferred = logical(),
    dataset_count = integer(),
    file_group_count = integer(),
    variable_count = integer(),
    file_count = integer(),
    declared_bytes = numeric(),
    known_file_count = integer(),
    unknown_file_count = integer(),
    declared_bytes_complete = logical(),
    file_group_ids = list()
  )
}

glc_plan_units_table <- function(units) {
  if (length(units) == 0L) {
    return(glc_plan_empty_units())
  }
  dplyr::bind_rows(lapply(units, function(unit) {
    tibble::tibble(
      unit_id = unit$unit_id,
      preferred = unit$preferred,
      dataset_count = unit$dataset_count,
      file_group_count = unit$file_group_count,
      variable_count = unit$variable_count,
      file_count = unit$file_count,
      declared_bytes = unit$declared_bytes,
      known_file_count = unit$known_file_count,
      unknown_file_count = unit$unknown_file_count,
      declared_bytes_complete = unit$declared_bytes_complete,
      file_group_ids = list(unit$file_group_ids)
    )
  }))
}

glc_plan_empty_groups <- function() {
  tibble::tibble(
    status = character(),
    unit_id = character(),
    dataset_id = character(),
    file_group = integer(),
    file_group_id = character(),
    study_id = character(),
    participant_id = character(),
    participant_associated = logical(),
    device_id = character(),
    device_location = character(),
    device_location_type = character(),
    description = character(),
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
    header_row = integer(),
    preprocessing = list(),
    datetime_source = character(),
    datetime_date = character(),
    datetime_format = character(),
    datetime_time = character(),
    datetime_time_format = character(),
    selected_variables = list(),
    reason_codes = list(),
    messages = list()
  )
}

glc_plan_groups_table <- function(records) {
  if (length(records) == 0L) {
    return(glc_plan_empty_groups())
  }
  dplyr::bind_rows(lapply(records, function(record) {
    selected_names <- vapply(
      record$selected_variables,
      function(variable) variable$name,
      character(1)
    )
    tibble::tibble(
      status = record$status,
      unit_id = record$unit_id,
      dataset_id = record$dataset_id,
      file_group = record$file_group,
      file_group_id = record$file_group_id,
      study_id = record$study_id,
      participant_id = record$participant_id,
      participant_associated = record$participant_associated,
      device_id = record$device_id,
      device_location = record$device_location,
      device_location_type = record$device_location_type,
      description = record$description,
      format = record$format,
      timezone = record$timezone,
      modalities = list(record$modalities),
      modality_other = record$modality_other,
      modality_other_type = record$modality_other_type,
      role = record$role,
      data_state = record$data_state,
      temporal_type = record$temporal_type,
      temporal_value = record$temporal_value,
      temporal_unit = record$temporal_unit,
      header_row = record$header_row,
      preprocessing = list(record$preprocessing),
      datetime_source = record$datetime$source,
      datetime_date = record$datetime$date,
      datetime_format = record$datetime$date_format,
      datetime_time = record$datetime$time,
      datetime_time_format = record$datetime$time_format,
      selected_variables = list(selected_names),
      reason_codes = list(vapply(
        record$reasons,
        function(reason) reason$code,
        character(1)
      )),
      messages = list(vapply(
        record$reasons,
        function(reason) reason$message,
        character(1)
      ))
    )
  }))
}

glc_plan_empty_variables <- function() {
  tibble::tibble(
    unit_id = character(),
    dataset_id = character(),
    file_group_id = character(),
    position = integer(),
    name = character(),
    label = character(),
    description = character(),
    unit = character(),
    calibration = character(),
    type = character(),
    term = character(),
    term_name = character(),
    primary = logical(),
    factor_values = list(),
    factor_labels = list(),
    factor_descriptions = list(),
    selection_origin = character()
  )
}

glc_plan_variables_table <- function(records, variable_scope) {
  rows <- list()
  for (record in records) {
    if (!identical(record$status, "included")) {
      next
    }
    for (position in seq_along(record$selected_variables)) {
      variable <- record$selected_variables[[position]]
      rows[[length(rows) + 1L]] <- tibble::tibble(
        unit_id = record$unit_id,
        dataset_id = record$dataset_id,
        file_group_id = record$file_group_id,
        position = position,
        name = variable$name,
        label = variable$label,
        description = variable$description,
        unit = variable$unit,
        calibration = variable$calibration,
        type = variable$type,
        term = variable$term,
        term_name = variable$term_name,
        primary = variable$primary,
        factor_values = list(vapply(
          variable$factor_levels,
          function(level) level$value,
          character(1)
        )),
        factor_labels = list(vapply(
          variable$factor_levels,
          function(level) level$label,
          character(1)
        )),
        factor_descriptions = list(vapply(
          variable$factor_levels,
          function(level) level$description,
          character(1)
        )),
        selection_origin = variable_scope
      )
    }
  }
  if (length(rows) == 0L) {
    return(glc_plan_empty_variables())
  }
  dplyr::bind_rows(rows)
}

glc_plan_empty_read_columns <- function() {
  tibble::tibble(
    unit_id = character(),
    dataset_id = character(),
    file_group_id = character(),
    position = integer(),
    name = character(),
    declared_type = character(),
    origin = character(),
    selected_for_output = logical(),
    automatic = logical(),
    message = character()
  )
}

glc_plan_read_columns_table <- function(records, variable_scope) {
  rows <- list()
  origin <- switch(
    variable_scope,
    matched = "matched_term",
    all = "all_declared",
    selected = "selected_name"
  )
  for (record in records) {
    if (!identical(record$status, "included")) {
      next
    }
    position <- 0L
    selected_names <- vapply(
      record$selected_variables,
      function(variable) variable$name,
      character(1)
    )
    for (variable in record$selected_variables) {
      position <- position + 1L
      rows[[length(rows) + 1L]] <- tibble::tibble(
        unit_id = record$unit_id,
        dataset_id = record$dataset_id,
        file_group_id = record$file_group_id,
        position = position,
        name = variable$name,
        declared_type = variable$type,
        origin = origin,
        selected_for_output = TRUE,
        automatic = FALSE,
        message = "Selected declared source column."
      )
    }
    datetime_columns <- character()
    if (identical(record$datetime$source, "column")) {
      datetime_columns <- c(datetime_columns, record$datetime$date)
      if (
        !is.na(record$datetime$time) &&
          nzchar(record$datetime$time)
      ) {
        datetime_columns <- c(datetime_columns, record$datetime$time)
      }
    }
    datetime_columns <- unique(datetime_columns)
    datetime_columns <- setdiff(datetime_columns, selected_names)
    declared_names <- vapply(
      record$variables,
      function(variable) variable$name,
      character(1)
    )
    for (name in datetime_columns) {
      variable <- record$variables[[match(name, declared_names)]]
      position <- position + 1L
      rows[[length(rows) + 1L]] <- tibble::tibble(
        unit_id = record$unit_id,
        dataset_id = record$dataset_id,
        file_group_id = record$file_group_id,
        position = position,
        name = name,
        declared_type = variable$type,
        origin = "datetime_required",
        selected_for_output = FALSE,
        automatic = TRUE,
        message = paste0(
          "Required to construct the declared datetime; not retained as a ",
          "source output column."
        )
      )
    }
  }
  if (length(rows) == 0L) {
    return(glc_plan_empty_read_columns())
  }
  dplyr::bind_rows(rows)
}

glc_plan_output_columns_for_unit <- function(unit, standardize) {
  columns <- lapply(unit$compatibility$variables, function(variable) {
    list(
      name = variable$name,
      source_declared_type = variable$type,
      expected_type = variable$type,
      origin = "selected",
      automatic = FALSE,
      runtime_validation_required = TRUE,
      collision_validation_required = FALSE,
      message = "Expected from the selected source declaration."
    )
  })
  automatic <- if (identical(standardize, "none")) {
    list(
      list(".glc_dataset_id", "string", "provenance"),
      list(".glc_file_group", "string", "provenance"),
      list(".glc_participant_id", "string", "provenance"),
      list(".glc_source_file", "string", "provenance"),
      list(".glc_datetime", "datetime", "parsed_datetime")
    )
  } else {
    list(
      list("Id", "factor", "lightlogr_standard"),
      list("file_group_id", "string", "lightlogr_standard"),
      list("participant_Id", "string", "lightlogr_standard"),
      list("Datetime", "datetime", "lightlogr_standard"),
      list("file.name", "string", "lightlogr_standard")
    )
  }
  for (definition in automatic) {
    name <- definition[[1L]]
    existing <- which(vapply(
      columns,
      function(column) {
        identical(column$name, name)
      },
      logical(1)
    ))
    if (length(existing) == 1L) {
      index <- existing[[1L]]
      columns[[index]]$origin <- paste0(
        columns[[index]]$origin,
        "_and_",
        definition[[3L]]
      )
      columns[[index]]$automatic <- TRUE
      columns[[index]]$expected_type <- definition[[2L]]
      columns[[index]]$collision_validation_required <- TRUE
      columns[[index]]$message <- paste0(
        "The source declaration collides with an automatically produced ",
        "column; glc_collect() validates the downloaded values."
      )
    } else {
      columns[[length(columns) + 1L]] <- list(
        name = name,
        source_declared_type = NA_character_,
        expected_type = definition[[2L]],
        origin = definition[[3L]],
        automatic = TRUE,
        runtime_validation_required = FALSE,
        collision_validation_required = FALSE,
        message = "Produced automatically after the source data are read."
      )
    }
  }
  dplyr::bind_rows(lapply(seq_along(columns), function(position) {
    column <- columns[[position]]
    tibble::tibble(
      unit_id = unit$unit_id,
      position = position,
      name = column$name,
      source_declared_type = column$source_declared_type,
      expected_type = column$expected_type,
      origin = column$origin,
      automatic = column$automatic,
      runtime_validation_required = column$runtime_validation_required,
      collision_validation_required = column$collision_validation_required,
      message = column$message
    )
  }))
}

glc_plan_empty_output_columns <- function() {
  tibble::tibble(
    unit_id = character(),
    position = integer(),
    name = character(),
    source_declared_type = character(),
    expected_type = character(),
    origin = character(),
    automatic = logical(),
    runtime_validation_required = logical(),
    collision_validation_required = logical(),
    message = character()
  )
}

glc_plan_output_columns_table <- function(units, standardize) {
  if (length(units) == 0L) {
    return(glc_plan_empty_output_columns())
  }
  dplyr::bind_rows(lapply(
    units,
    glc_plan_output_columns_for_unit,
    standardize = standardize
  ))
}

glc_plan_empty_files <- function() {
  tibble::tibble(
    status = character(),
    unit_id = character(),
    dataset_id = character(),
    file_group_id = character(),
    position = integer(),
    declared_path = character(),
    format = character(),
    encoding = character(),
    declared_bytes = numeric(),
    bytes_known = logical()
  )
}

glc_plan_files_table <- function(records) {
  rows <- list()
  for (record in records) {
    for (position in seq_along(record$files)) {
      file <- record$files[[position]]
      rows[[length(rows) + 1L]] <- tibble::tibble(
        status = record$status,
        unit_id = record$unit_id,
        dataset_id = record$dataset_id,
        file_group_id = record$file_group_id,
        position = position,
        declared_path = file$declared_path,
        format = record$format,
        encoding = file$encoding,
        declared_bytes = file$declared_bytes,
        bytes_known = !is.na(file$declared_bytes)
      )
    }
  }
  if (length(rows) == 0L) {
    return(glc_plan_empty_files())
  }
  dplyr::bind_rows(rows)
}

glc_plan_empty_compatibility <- function() {
  tibble::tibble(
    unit_id = character(),
    selected_names = list(),
    declared_types = list(),
    factor_values = list(),
    factor_labels = list(),
    timezone = character(),
    modalities = list(),
    role = character(),
    data_state = character(),
    datetime_source = character(),
    datetime_signature = character(),
    datetime_date = character(),
    datetime_format = character(),
    datetime_time = character(),
    datetime_time_format = character(),
    collection_values_ignored = logical(),
    relationship_rule = character(),
    device_rule = character(),
    standardize = character(),
    standardize_affects_partition = logical()
  )
}

glc_plan_compatibility_table <- function(units, standardize) {
  if (length(units) == 0L) {
    return(glc_plan_empty_compatibility())
  }
  dplyr::bind_rows(lapply(units, function(unit) {
    contract <- unit$compatibility
    tibble::tibble(
      unit_id = unit$unit_id,
      selected_names = list(vapply(
        contract$variables,
        function(variable) variable$name,
        character(1)
      )),
      declared_types = list(vapply(
        contract$variables,
        function(variable) variable$type,
        character(1)
      )),
      factor_values = list(lapply(
        contract$variables,
        function(variable) variable$factor_values
      )),
      factor_labels = list(lapply(
        contract$variables,
        function(variable) variable$factor_labels
      )),
      timezone = contract$timezone,
      modalities = list(contract$modalities),
      role = contract$role,
      data_state = contract$data_state,
      datetime_source = contract$datetime$source,
      datetime_signature = contract$datetime$signature,
      datetime_date = contract$datetime$date,
      datetime_format = contract$datetime$date_format,
      datetime_time = contract$datetime$time,
      datetime_time_format = contract$datetime$time_format,
      collection_values_ignored = identical(
        contract$datetime$source,
        "collection"
      ),
      relationship_rule = contract$relationship_rule,
      device_rule = contract$device_rule,
      standardize = standardize,
      standardize_affects_partition = FALSE
    )
  }))
}

glc_plan_extensions_table <- function(records) {
  if (length(records) == 0L) {
    return(tibble::tibble(
      dataset_id = character(),
      file_group_id = character(),
      metadata = list()
    ))
  }
  dplyr::bind_rows(lapply(records, function(record) {
    tibble::tibble(
      dataset_id = record$dataset_id,
      file_group_id = record$file_group_id,
      metadata = list(record$extensions)
    )
  }))
}

glc_plan_known_values <- function(records, field) {
  glc_unique_chr(lapply(records, function(record) {
    if (identical(field, "term")) {
      vapply(
        record$variables,
        function(variable) {
          variable$term
        },
        character(1)
      )
    } else {
      vapply(
        record$variables,
        function(variable) {
          variable$name
        },
        character(1)
      )
    }
  }))
}

glc_plan_validate_restrictions <- function(records, request) {
  dataset_ids <- unique(vapply(
    records,
    function(record) record$dataset_id,
    character(1)
  ))
  file_group_ids <- vapply(
    records,
    function(record) record$file_group_id,
    character(1)
  )
  unknown_datasets <- setdiff(request$dataset_id, dataset_ids)
  if (length(unknown_datasets) > 0L) {
    glc_abort(
      "Unknown dataset id{?s}: {.val {unknown_datasets}}.",
      class = "glcdp_unknown_dataset"
    )
  }
  unknown_groups <- setdiff(request$file_group, file_group_ids)
  if (length(unknown_groups) > 0L) {
    glc_abort(
      "Unknown stable file-group id{?s}: {.val {unknown_groups}}.",
      class = "glcdp_unknown_file_group"
    )
  }
  known_terms <- glc_plan_known_values(records, "term")
  unknown_terms <- setdiff(request$terms, known_terms)
  if (length(unknown_terms) > 0L) {
    glc_abort(
      paste0(
        "Unknown canonical semantic term{?s}: {.val {unknown_terms}}. ",
        "Labels are display metadata and are not term identifiers."
      ),
      class = "glcdp_unknown_term"
    )
  }
  known_variables <- glc_plan_known_values(records, "name")
  unknown_variables <- setdiff(
    request$requested_variables,
    known_variables
  )
  if (length(unknown_variables) > 0L) {
    glc_abort(
      "Unknown declared source variable{?s}: {.val {unknown_variables}}.",
      class = "glcdp_unknown_variable"
    )
  }
  invisible(TRUE)
}

#' Plan declaration-compatible collection units
#'
#' Build a deterministic, metadata-only plan that partitions matching file
#' groups into units whose validated declarations are compatible for collection.
#' No measurement file is read or inspected while the plan is built.
#'
#' @param x A `glc_package` opened with [glc_open()] at an exact, verified
#'   revision. A remote package must be at the registry's latest passing
#'   revision. A local package must be manifest-backed, with the same exact
#'   revision and `registry_verified = true`.
#' @param terms Optional exact canonical semantic-term identifiers. Labels and
#'   other display text are not identifiers. `terms` is required when
#'   `variable_scope = "matched"`.
#' @param variable_scope Which declared source variables to plan:
#'
#'   * `"matched"` selects every variable whose canonical term is one of
#'     `terms`;
#'   * `"all"` selects all declared variables in each candidate file group;
#'   * `"selected"` selects the exact source names supplied in `variables`.
#' @param variables Exact declared source-variable names. This argument is
#'   required for `variable_scope = "selected"` and must otherwise be `NULL`.
#' @param dataset_id Optional exact dataset identifiers restricting the
#'   candidate groups.
#' @param file_group Optional stable file-group identifiers, such as
#'   `"DS1:1"`, restricting the candidate groups. Numeric group indices are
#'   deliberately not accepted because they are not stable identifiers.
#' @param standardize Expected collection output convention. `"lightlogr"`
#'   plans the columns produced by `glc_collect(standardize = "lightlogr")`;
#'   `"none"` plans the unstandardized provenance columns. This choice changes
#'   expected output columns and unit identifiers, but not the declaration
#'   compatibility partition.
#'
#' @details
#' `terms` always acts as the file-group discovery predicate. A candidate group
#' must declare every requested term, while one term may be declared by more
#' than one variable. With `variable_scope = "matched"`, all variables carrying
#' any requested term are selected. With `"all"`, all declared variables are
#' selected after the optional term predicate is applied. With `"selected"`,
#' `terms` remains an optional, independent discovery predicate and every
#' requested source name must be declared by a group. Input order does not
#' affect the result; selected variables retain declaration order.
#'
#' `dataset_id` and `file_group` are intersecting restrictions. Omitting a
#' restriction and explicitly supplying every possible identifier select the
#' same groups, but deliberately remain different requests and therefore may
#' produce different unit identifiers.
#'
#' Included groups are partitioned by the exact selected variable names and
#' order, declared types, factor values and labels, time zone, ordered
#' modalities, role, data state, datetime contract, and the package's validated
#' relationship rules. At most one non-missing device per dataset is permitted
#' within a unit. Collection-based datetime values are record-specific and are
#' ignored when comparing otherwise identical collection-based datetime
#' contracts.
#'
#' A unit identifier is `"glcu_"` followed by a SHA-256 digest of canonical
#' UTF-8 text. The material includes the planner schema and version, package id,
#' repository, exact source revision and package schema, the normalized request
#' (including restrictions and `standardize`), the compatibility contract, and
#' sorted stable file-group identifiers. It never uses R serialized-object
#' bytes. Unit identifiers and table ordering are therefore reproducible across
#' input row ordering and supported R versions. They are request- and
#' revision-specific and may change when the planner schema changes.
#'
#' @section Declaration-only assurance:
#' The planner uses validated descriptor and core metadata associated with
#' `x`. It does not call [glc_read()], [glc_collect()], [glc_files()], or
#' [glc_summary()], request measurement contents, or inspect source rows.
#' Known declaration-level constraints, including reserved source names that
#' begin with `.glc_`, are applied before units are formed.
#'
#' File sizes come only from an explicit supported byte declaration or an entry
#' already present in the local manifest. The planner never downloads a file or
#' probes a remote object to discover its size; unavailable sizes remain `NA`.
#' A unit's `declared_bytes` is the sum of known sizes, and
#' `declared_bytes_complete` records whether every file size is known.
#'
#' Compatibility is an assurance about validated declarations, not downloaded
#' values. Actual columns, parsed classes, factor values, datetime values, and
#' output-column collisions can only be checked after reading. [glc_read()]
#' remains authoritative for source-file validation, and [glc_collect()] remains
#' authoritative for final collection compatibility and standardization.
#'
#' @section Return tables:
#' The result is a plain, serializable list with class `glc_collection_plan` and
#' these components:
#'
#' * `plan_schema` and `plan_version`: the top-level schema id
#'   `"glc-collection-plan"` and its semantic version.
#' * `provenance`: `package_id`, `repository`, `source_type`, exact
#'   `source_revision`, `package_schema_version`, `verification`,
#'   `latest_pass_commit`, `registry_generated_at`, `manifest_version`,
#'   declaration `metadata_fingerprint`, `planner_schema`, and
#'   `planner_version`. It contains no package handle, token, cache path, or
#'   temporary path.
#' * `request`: normalized `terms`, fixed `term_match = "all"`,
#'   `term_identifier = "canonical"`, `labels_used_for_matching = FALSE`,
#'   `variable_scope`, `requested_variables`, `dataset_id`, `file_group`,
#'   `standardize`, and the sorted union `resolved_variables` from included
#'   groups.
#' * `assurance`: `basis`, `actual_data_status`, `final_validation`,
#'   `measurement_contents_transferred`, `measurement_contents_inspected`, and
#'   `byte_policy`.
#' * `preferred_unit_id`: the preferred unit, or `NA_character_` when no unit is
#'   collectable. Preference is deterministic: most datasets, then most file
#'   groups, then the lexically smallest unit id.
#' * `units`: one row per collectable unit. Columns are `unit_id`, `preferred`,
#'   `dataset_count`, `file_group_count`, `variable_count`, `file_count`,
#'   `declared_bytes`, `known_file_count`, `unknown_file_count`,
#'   `declared_bytes_complete`, and the list-column `file_group_ids`.
#' * `groups`: one row per declared file group. Columns are `status`, `unit_id`,
#'   `dataset_id`, integer declaration index `file_group`, stable
#'   `file_group_id`, `study_id`, `participant_id`, `participant_associated`,
#'   `device_id`, `device_location`, `device_location_type`, `description`,
#'   `format`, `timezone`, list-column `modalities`, `modality_other`,
#'   `modality_other_type`, `role`, `data_state`, `temporal_type`,
#'   `temporal_value`, `temporal_unit`, `header_row`, list-column
#'   `preprocessing`, `datetime_source`, `datetime_date`, `datetime_format`,
#'   `datetime_time`, `datetime_time_format`, and list-columns
#'   `selected_variables`, `reason_codes`, and `messages`. Excluded groups have
#'   a missing `unit_id`.
#' * `variables`: one row per selected variable in an included group. Columns
#'   are `unit_id`, `dataset_id`, `file_group_id`, `position`, `name`, `label`,
#'   `description`, `unit`, `calibration`, `type`, canonical `term`,
#'   `term_name`, `primary`, list-columns `factor_values`, `factor_labels`, and
#'   `factor_descriptions`, and `selection_origin`.
#' * `read_columns`: one row per selected or automatically required source
#'   column. Columns are `unit_id`, `dataset_id`, `file_group_id`, `position`,
#'   `name`, `declared_type`, `origin`, `selected_for_output`, `automatic`, and
#'   `message`. Datetime source columns needed only for parsing are automatic
#'   read columns, not requested output variables.
#' * `output_columns`: one row per expected post-collection column and unit.
#'   Columns are `unit_id`, `position`, `name`, `source_declared_type`,
#'   `expected_type`, `origin`, `automatic`, `runtime_validation_required`,
#'   `collision_validation_required`, and `message`. These expectations are
#'   still subject to [glc_collect()] validation.
#' * `files`: one row per declared file in included or excluded groups. Columns
#'   are `status`, `unit_id`, `dataset_id`, `file_group_id`, `position`,
#'   `declared_path`, `format`, `encoding`, `declared_bytes`, and `bytes_known`.
#' * `compatibility`: one row per unit. Columns are `unit_id`, list-columns
#'   `selected_names`, `declared_types`, `factor_values`, and `factor_labels`,
#'   `timezone`, list-column `modalities`, `role`, `data_state`,
#'   `datetime_source`, `datetime_signature`, `datetime_date`,
#'   `datetime_format`, `datetime_time`, `datetime_time_format`,
#'   `collection_values_ignored`, `relationship_rule`, `device_rule`,
#'   `standardize`, and the fixed `standardize_affects_partition = FALSE`
#'   assurance.
#' * `extensions`: one row per group. Columns are `dataset_id`,
#'   `file_group_id`, and preserved, forward-compatible unknown declaration
#'   fields in the plain list-column `metadata`.
#'
#' All tables are tibbles with stable columns, including when they have no rows.
#' List-columns contain only plain serializable vectors and lists. `print()`
#' shows a compact package, request, unit, byte, preferred-unit, and assurance
#' summary and returns the plan invisibly.
#'
#' @section Exclusions and errors:
#' Per-group declaration outcomes are returned rather than thrown. Stable reason
#' codes are `included`, `scope_dataset`, `scope_file_group`,
#' `reserved_provenance_column`, `term_missing`, `variable_missing`,
#' `no_declared_files`, `unsupported_format`, `invalid_timezone`, and
#' `incomplete_datetime`; each has a plain-language message.
#'
#' Invalid argument types, empty values, duplicates, and inconsistent
#' `variable_scope`/selector combinations error before planning. Programmatically
#' useful condition subclasses include `glcdp_unknown_dataset`,
#' `glcdp_unknown_file_group`, `glcdp_unknown_term`, `glcdp_unknown_variable`,
#' `glcdp_collection_plan_revision`, `glcdp_collection_plan_provenance`,
#' `glcdp_collection_plan_ambiguous_variable`,
#' `glcdp_collection_plan_relationship`, `glcdp_collection_plan_serialization`,
#' and `glcdp_collection_plan_id`. Term labels that are not canonical ids are
#' reported as unknown terms rather than matched ambiguously.
#'
#' @return A `glc_collection_plan` object described in **Return tables**.
#' @seealso [glc_variables()] for declared selectors, [glc_read()] for runtime
#'   import validation, and [glc_collect()] for authoritative final collection.
#' @export
#'
#' @examples
#' \dontrun{
#' # Use an existing local, manifest-backed directory created by glc_download().
#' # This pattern performs no network request and does not read measurements.
#' pkg <- glc_open("path/to/manifest-backed-package", quiet = TRUE)
#' plan <- glc_collection_plan(
#'   pkg,
#'   terms = "photopic illuminance",
#'   variable_scope = "matched"
#' )
#' plan
#' plan$units
#' plan$groups[, c("file_group_id", "status", "reason_codes")]
#' }
glc_collection_plan <- function(
  x,
  terms = NULL,
  variable_scope = c("matched", "all", "selected"),
  variables = NULL,
  dataset_id = NULL,
  file_group = NULL,
  standardize = c("lightlogr", "none")
) {
  glc_assert_package(x)
  variable_scope <- match.arg(variable_scope)
  standardize <- match.arg(standardize)
  terms <- glc_plan_character_argument(terms, "terms")
  variables <- glc_plan_character_argument(variables, "variables")
  dataset_id <- glc_plan_character_argument(dataset_id, "dataset_id")
  file_group <- glc_plan_character_argument(file_group, "file_group")

  if (identical(variable_scope, "matched") && length(terms) == 0L) {
    glc_abort(
      "{.arg terms} is required when {.arg variable_scope} is {.val matched}."
    )
  }
  if (
    !identical(variable_scope, "selected") &&
      length(variables) > 0L
  ) {
    glc_abort(
      paste0(
        "{.arg variables} must be empty unless {.arg variable_scope} is ",
        "{.val selected}."
      )
    )
  }
  if (
    identical(variable_scope, "selected") &&
      length(variables) == 0L
  ) {
    glc_abort(
      "{.arg variables} is required when {.arg variable_scope} is {.val selected}."
    )
  }

  provenance <- glc_plan_verified_provenance(x)
  records <- glc_plan_group_records(x)
  request <- list(
    terms = glc_plan_sort_utf8(terms),
    term_match = "all",
    term_identifier = "canonical",
    labels_used_for_matching = FALSE,
    variable_scope = variable_scope,
    requested_variables = glc_plan_sort_utf8(variables),
    dataset_id = glc_plan_sort_utf8(dataset_id),
    file_group = glc_plan_sort_utf8(file_group),
    standardize = standardize
  )
  glc_plan_validate_restrictions(records, request)
  metadata_fingerprint <- glc_plan_digest(list(
    package = provenance,
    declarations = lapply(records, glc_plan_declaration_snapshot)
  ))
  engine <- glc_declared_collection_engine(
    records,
    request,
    provenance
  )
  records <- engine$records
  request$resolved_variables <- glc_plan_sort_utf8(unique(unlist(
    lapply(records, function(record) {
      if (!identical(record$status, "included")) {
        return(character())
      }
      vapply(
        record$selected_variables,
        function(variable) {
          variable$name
        },
        character(1)
      )
    }),
    use.names = FALSE
  )))

  provenance$metadata_fingerprint <- metadata_fingerprint
  provenance$planner_schema <- glc_collection_plan_schema()
  provenance$planner_version <- glc_collection_plan_version()
  structure(
    list(
      plan_schema = glc_collection_plan_schema(),
      plan_version = glc_collection_plan_version(),
      provenance = provenance,
      request = request,
      assurance = list(
        basis = "validated_declarations",
        actual_data_status = "not_checked",
        final_validation = c("glc_read", "glc_collect"),
        measurement_contents_transferred = FALSE,
        measurement_contents_inspected = FALSE,
        byte_policy = paste0(
          "Only explicit supported declaration bytes or entries in the ",
          "already-loaded manifest are used; unknown sizes remain unknown."
        )
      ),
      preferred_unit_id = engine$preferred_unit_id,
      units = glc_plan_units_table(engine$units),
      groups = glc_plan_groups_table(records),
      variables = glc_plan_variables_table(records, variable_scope),
      read_columns = glc_plan_read_columns_table(records, variable_scope),
      output_columns = glc_plan_output_columns_table(
        engine$units,
        standardize
      ),
      files = glc_plan_files_table(records),
      compatibility = glc_plan_compatibility_table(
        engine$units,
        standardize
      ),
      extensions = glc_plan_extensions_table(records)
    ),
    class = c("glc_collection_plan", "list")
  )
}

#' @export
print.glc_collection_plan <- function(x, ...) {
  included <- sum(x$groups$status == "included")
  excluded <- sum(x$groups$status == "excluded")
  included_files <- x$files$status == "included"
  known_files <- included_files & x$files$bytes_known
  known_bytes <- sum(x$files$declared_bytes[known_files], na.rm = TRUE)
  unknown_files <- sum(included_files & !x$files$bytes_known)
  terms <- if (length(x$request$terms) == 0L) {
    "none"
  } else {
    paste(x$request$terms, collapse = ", ")
  }
  preferred <- if (is.na(x$preferred_unit_id)) {
    "none"
  } else {
    x$preferred_unit_id
  }
  cat("<GLC collection plan>\n")
  cat(
    "Package: ",
    x$provenance$package_id,
    "@",
    substr(x$provenance$source_revision, 1L, 12L),
    "\n",
    sep = ""
  )
  cat(
    "Schema: ",
    x$plan_schema,
    " ",
    x$plan_version,
    " (package ",
    x$provenance$package_schema_version,
    ")\n",
    sep = ""
  )
  cat(
    "Request: all-of terms [",
    terms,
    "]; variable scope ",
    x$request$variable_scope,
    "\n",
    sep = ""
  )
  cat(
    "Result: ",
    nrow(x$units),
    " unit(s); ",
    included,
    " included, ",
    excluded,
    " excluded group(s)\n",
    sep = ""
  )
  cat(
    "Declared bytes: ",
    format(known_bytes, scientific = FALSE, trim = TRUE),
    " known; ",
    unknown_files,
    " file(s) unknown\n",
    sep = ""
  )
  cat("Preferred unit: ", preferred, "\n", sep = "")
  cat(
    "Assurance: validated declarations only; downloaded data not checked\n"
  )
  invisible(x)
}
