glc_collection_plan_schema <- function() {
  "glc-collection-plan"
}

glc_collection_plan_version <- function() {
  "1.2.0"
}

glc_collection_unit_id_schema <- function() {
  "glc-collection-plan"
}

glc_collection_unit_id_version <- function() {
  "2.0.0"
}

glc_collection_compatibility_id_schema <- function() {
  "glc-collection-compatibility"
}

glc_collection_compatibility_id_version <- function() {
  "2.0.0"
}

glc_plan_valid_timezones <- function() {
  OlsonNames()
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
      fields <- lapply(order_index, function(index) {
        c(
          glc_plan_length_token("key", object_names[[index]]),
          glc_plan_canonical_tokens(x[[index]])
        )
      })
      return(c(
        glc_plan_length_token("object", length(x)),
        unlist(fields, use.names = FALSE)
      ))
    }
    return(c(
      glc_plan_length_token("array", length(x)),
      unlist(lapply(x, glc_plan_canonical_tokens), use.names = FALSE)
    ))
  }
  if (is.character(x)) {
    values <- vapply(
      x,
      function(value) {
        if (is.na(value)) {
          "character-na:0:"
        } else {
          glc_plan_length_token("character-value", value)
        }
      },
      character(1)
    )
    return(c(glc_plan_length_token("character", length(x)), values))
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
    "dataset_file_preprocessing",
    "dataset_file_instrument"
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
        dataset_timezone = dataset$timezone,
        dataset_latitude = dataset$latitude,
        dataset_longitude = dataset$longitude,
        file_group = group$index,
        file_group_id = group$id,
        description = group$description,
        instructions = group$instructions,
        instrument = glc_plan_safe_metadata(group$instrument),
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
        study_link_status = NA_character_,
        participant_link_status = NA_character_,
        device_link_status = NA_character_,
        datasheet_id = NA_character_,
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
  variables <- lapply(
    record$selected_variables,
    glc_factor_contract_variable
  )
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
    relationship_rule = "consistent_file_group_and_dataset_relationships",
    device_rule = "device_identity_is_file_group_scoped"
  )
}

glc_plan_add_reason <- function(reasons, code, message) {
  c(reasons, list(glc_plan_reason(code, message)))
}

glc_plan_evaluate_group <- function(record, request, valid_timezones) {
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
  if (in_scope && length(selected_variables) > 0L) {
    factor_issues <- lapply(selected_variables, function(variable) {
      glc_factor_contract_issue(glc_factor_contract_variable(variable))
    })
    invalid <- which(!vapply(factor_issues, is.null, logical(1)))
    if (length(invalid) > 0L) {
      issue <- factor_issues[[invalid[[1L]]]]
      reasons <- glc_plan_add_reason(
        reasons,
        "invalid_factor_contract",
        issue$message
      )
    }
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
      (is.na(record$timezone) || !record$timezone %in% valid_timezones)
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
  record$compatibility_id <- NA_character_
  record
}

glc_plan_unit_id <- function(
  compatibility,
  file_group_ids,
  request,
  provenance
) {
  material <- list(
    plan_schema = glc_collection_unit_id_schema(),
    plan_version = glc_collection_unit_id_version(),
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

glc_plan_compatibility_id <- function(compatibility, provenance) {
  material <- list(
    id_schema = glc_collection_compatibility_id_schema(),
    id_version = glc_collection_compatibility_id_version(),
    package = list(
      package_id = provenance$package_id,
      repository = provenance$repository,
      source_revision = provenance$source_revision,
      package_schema_version = provenance$package_schema_version
    ),
    contract = compatibility
  )
  paste0("glcc_", glc_plan_digest(material))
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

glc_declared_collection_partition <- function(
  records,
  request,
  provenance,
  diagnostics = list()
) {
  included <- which(vapply(
    records,
    function(record) {
      identical(record$status, "included")
    },
    logical(1)
  ))
  units <- list()
  compatibility_sets <- list()

  if (length(included) > 0L) {
    family_ids <- vapply(
      records[included],
      function(record) {
        record$family_compatibility_id %||% record$compatibility_id
      },
      character(1)
    )
    if (anyNA(family_ids) || any(!nzchar(family_ids))) {
      glc_abort(
        "Collection planning could not resolve structural family identifiers.",
        class = "glcdp_collection_plan_id"
      )
    }
    for (compatibility_id in unique(family_ids)) {
      bucket_indexes <- included[family_ids == compatibility_id]
      declared_contracts <- lapply(records[bucket_indexes], function(record) {
        record$declared_compatibility %||%
          glc_plan_declared_compatibility(record)
      })
      merged <- glc_compatibility_merge_contracts(declared_contracts)
      if (!isTRUE(merged$compatible)) {
        glc_abort(
          "A structural family no longer has a compatible active declaration contract.",
          class = "glcdp_collection_plan_contract"
        )
      }
      contract <- merged$contract
      family_contract <- records[[bucket_indexes[[
        1L
      ]]]]$family_compatibility %||%
        contract
      harmonized_variables <- vapply(
        merged$results[vapply(
          merged$results,
          function(result) {
            isTRUE(result$harmonization_required)
          },
          logical(1)
        )],
        function(result) result$variable_name,
        character(1)
      )
      member_ids <- glc_plan_sort_utf8(vapply(
        records[bucket_indexes],
        function(record) record$file_group_id,
        character(1)
      ))
      diagnostic_ids <- if (length(diagnostics) == 0L) {
        glc_plan_sort_utf8(unique(unlist(lapply(
          records[bucket_indexes],
          function(record) record$diagnostic_ids %||% character()
        ))))
      } else {
        glc_plan_sort_utf8(vapply(
          diagnostics[vapply(
            diagnostics,
            function(diagnostic) {
              length(intersect(
                diagnostic$affected_file_group_ids,
                member_ids
              )) >
                0L
            },
            logical(1)
          )],
          function(diagnostic) {
            diagnostic$diagnostic_id
          },
          character(1)
        ))
      }
      for (bucket_index in bucket_indexes) {
        records[[bucket_index]]$compatibility_id <- compatibility_id
        records[[bucket_index]]$active_compatibility <- contract
        records[[bucket_index]]$diagnostic_ids <- diagnostic_ids
      }
      unit_id <- glc_plan_unit_id(
        contract,
        member_ids,
        request,
        provenance
      )
      for (member_index in bucket_indexes) {
        records[[member_index]]$unit_id <- unit_id
      }
      member_records <- records[bucket_indexes]
      byte_summary <- glc_plan_unit_byte_summary(member_records)
      units[[length(units) + 1L]] <- c(
        list(
          unit_id = unit_id,
          compatibility_id = compatibility_id,
          file_group_ids = member_ids,
          compatibility = contract,
          dataset_count = length(unique(vapply(
            member_records,
            function(record) record$dataset_id,
            character(1)
          ))),
          file_group_count = length(member_records),
          variable_count = length(contract$variables),
          harmonization_required = length(harmonized_variables) > 0L,
          harmonized_variables = harmonized_variables,
          diagnostic_ids = diagnostic_ids
        ),
        byte_summary
      )
      set_records <- records[bucket_indexes]
      compatibility_sets[[length(compatibility_sets) + 1L]] <- c(
        list(
          compatibility_id = compatibility_id,
          file_group_ids = member_ids,
          compatibility = family_contract,
          dataset_count = length(unique(vapply(
            set_records,
            function(record) record$dataset_id,
            character(1)
          ))),
          file_group_count = length(set_records),
          variable_count = length(family_contract$variables),
          harmonization_required = length(harmonized_variables) > 0L,
          harmonized_variables = harmonized_variables,
          diagnostic_ids = diagnostic_ids
        ),
        glc_plan_unit_byte_summary(set_records)
      )
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

  if (length(compatibility_sets) > 0L) {
    compatibility_ids <- vapply(
      compatibility_sets,
      function(set) set$compatibility_id,
      character(1)
    )
    if (anyDuplicated(compatibility_ids)) {
      glc_abort(
        "Collection planning produced duplicate compatibility identifiers.",
        class = "glcdp_collection_plan_id"
      )
    }
    compatibility_sets <- compatibility_sets[order(
      glc_plan_utf8_key(compatibility_ids),
      method = "radix"
    )]
    compatibility_sets <- lapply(compatibility_sets, function(set) {
      final_unit_ids <- glc_plan_sort_utf8(vapply(
        units[vapply(
          units,
          function(unit)
            identical(
              unit$compatibility_id,
              set$compatibility_id
            ),
          logical(1)
        )],
        function(unit) unit$unit_id,
        character(1)
      ))
      set$final_unit_ids <- final_unit_ids
      set$final_unit_count <- length(final_unit_ids)
      set$final_selection_required <- FALSE
      set$constraint_codes <- character()
      set$constraint_messages <- character()
      set
    })
  }

  list(
    records = records,
    units = units,
    compatibility_sets = compatibility_sets,
    preferred_unit_id = preferred_unit_id,
    diagnostics = diagnostics
  )
}

glc_declared_collection_engine <- function(records, request, provenance) {
  valid_timezones <- glc_plan_valid_timezones()
  family_request <- request
  family_request$dataset_id <- character()
  family_request$file_group <- character()
  family_records <- lapply(
    records,
    glc_plan_evaluate_group,
    request = family_request,
    valid_timezones = valid_timezones
  )
  families <- glc_collection_compatibility_families(
    family_records,
    request,
    provenance
  )
  records <- lapply(
    records,
    glc_plan_evaluate_group,
    request = request,
    valid_timezones = valid_timezones
  )
  family_ids <- vapply(
    families$records,
    function(record) {
      record$file_group_id
    },
    character(1)
  )
  for (index in seq_along(records)) {
    family <- families$records[[match(
      records[[index]]$file_group_id,
      family_ids
    )]]
    records[[index]]$declared_compatibility <-
      family$declared_compatibility %||%
      glc_plan_declared_compatibility(records[[index]])
    records[[index]]$family_compatibility_id <-
      family$family_compatibility_id %||% NA_character_
    records[[index]]$family_compatibility <- family$family_compatibility
  }
  glc_declared_collection_partition(
    records,
    request,
    provenance,
    diagnostics = families$diagnostics
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
  glc_declared_collection_contract_differences(contracts)
}

glc_declared_collection_record_differences <- function(records) {
  if (length(records) < 2L) {
    return(character())
  }
  contracts <- lapply(records, function(record) {
    record$active_compatibility %||%
      record$declared_compatibility %||%
      glc_plan_declared_compatibility(record)
  })
  glc_declared_collection_contract_differences(contracts)
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
    dataset_timezone = record$dataset_timezone,
    dataset_latitude = record$dataset_latitude,
    dataset_longitude = record$dataset_longitude,
    file_group = record$file_group,
    file_group_id = record$file_group_id,
    description = record$description,
    instructions = record$instructions,
    instrument = record$instrument,
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
    compatibility_id = character(),
    preferred = logical(),
    dataset_count = integer(),
    file_group_count = integer(),
    variable_count = integer(),
    file_count = integer(),
    declared_bytes = numeric(),
    known_file_count = integer(),
    unknown_file_count = integer(),
    declared_bytes_complete = logical(),
    harmonization_required = logical(),
    harmonized_variables = list(),
    diagnostic_ids = list(),
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
      compatibility_id = unit$compatibility_id,
      preferred = unit$preferred,
      dataset_count = unit$dataset_count,
      file_group_count = unit$file_group_count,
      variable_count = unit$variable_count,
      file_count = unit$file_count,
      declared_bytes = unit$declared_bytes,
      known_file_count = unit$known_file_count,
      unknown_file_count = unit$unknown_file_count,
      declared_bytes_complete = unit$declared_bytes_complete,
      harmonization_required = unit$harmonization_required,
      harmonized_variables = list(unit$harmonized_variables),
      diagnostic_ids = list(unit$diagnostic_ids),
      file_group_ids = list(unit$file_group_ids)
    )
  }))
}

glc_plan_empty_groups <- function() {
  tibble::tibble(
    status = character(),
    unit_id = character(),
    compatibility_id = character(),
    dataset_id = character(),
    file_group = integer(),
    file_group_id = character(),
    study_id = character(),
    participant_id = character(),
    participant_associated = logical(),
    study_link_status = character(),
    participant_link_status = character(),
    device_id = character(),
    device_link_status = character(),
    datasheet_id = character(),
    device_location = character(),
    device_location_type = character(),
    description = character(),
    instructions = character(),
    instrument_declared = logical(),
    dataset_timezone = character(),
    dataset_latitude = numeric(),
    dataset_longitude = numeric(),
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
  character_field <- function(field) {
    vapply(records, function(record) record[[field]], character(1))
  }
  tibble::tibble(
    status = character_field("status"),
    unit_id = character_field("unit_id"),
    compatibility_id = character_field("compatibility_id"),
    dataset_id = character_field("dataset_id"),
    file_group = vapply(
      records,
      function(record) record$file_group,
      integer(1)
    ),
    file_group_id = character_field("file_group_id"),
    study_id = character_field("study_id"),
    participant_id = character_field("participant_id"),
    participant_associated = vapply(
      records,
      function(record) record$participant_associated,
      logical(1)
    ),
    study_link_status = character_field("study_link_status"),
    participant_link_status = character_field("participant_link_status"),
    device_id = character_field("device_id"),
    device_link_status = character_field("device_link_status"),
    datasheet_id = character_field("datasheet_id"),
    device_location = character_field("device_location"),
    device_location_type = character_field("device_location_type"),
    description = character_field("description"),
    instructions = character_field("instructions"),
    instrument_declared = vapply(
      records,
      function(record) length(record$instrument) > 0L,
      logical(1)
    ),
    dataset_timezone = character_field("dataset_timezone"),
    dataset_latitude = vapply(
      records,
      function(record) record$dataset_latitude,
      numeric(1)
    ),
    dataset_longitude = vapply(
      records,
      function(record) record$dataset_longitude,
      numeric(1)
    ),
    format = character_field("format"),
    timezone = character_field("timezone"),
    modalities = lapply(records, function(record) record$modalities),
    modality_other = character_field("modality_other"),
    modality_other_type = character_field("modality_other_type"),
    role = character_field("role"),
    data_state = character_field("data_state"),
    temporal_type = character_field("temporal_type"),
    temporal_value = vapply(
      records,
      function(record) record$temporal_value,
      numeric(1)
    ),
    temporal_unit = character_field("temporal_unit"),
    header_row = vapply(
      records,
      function(record) record$header_row,
      integer(1)
    ),
    preprocessing = lapply(records, function(record) record$preprocessing),
    datetime_source = vapply(
      records,
      function(record) record$datetime$source,
      character(1)
    ),
    datetime_date = vapply(
      records,
      function(record) record$datetime$date,
      character(1)
    ),
    datetime_format = vapply(
      records,
      function(record) record$datetime$date_format,
      character(1)
    ),
    datetime_time = vapply(
      records,
      function(record) record$datetime$time,
      character(1)
    ),
    datetime_time_format = vapply(
      records,
      function(record) record$datetime$time_format,
      character(1)
    ),
    selected_variables = lapply(records, function(record) {
      vapply(
        record$selected_variables,
        function(variable) variable$name,
        character(1)
      )
    }),
    reason_codes = lapply(records, function(record) {
      vapply(record$reasons, function(reason) reason$code, character(1))
    }),
    messages = lapply(records, function(record) {
      vapply(record$reasons, function(reason) reason$message, character(1))
    })
  )
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
  rows <- unlist(
    lapply(records, function(record) {
      if (!identical(record$status, "included")) {
        return(list())
      }
      lapply(seq_along(record$selected_variables), function(position) {
        list(
          unit_id = record$unit_id,
          dataset_id = record$dataset_id,
          file_group_id = record$file_group_id,
          position = as.integer(position),
          variable = record$selected_variables[[position]]
        )
      })
    }),
    recursive = FALSE
  )
  if (length(rows) == 0L) {
    return(glc_plan_empty_variables())
  }
  tibble::tibble(
    unit_id = vapply(rows, function(row) row$unit_id, character(1)),
    dataset_id = vapply(rows, function(row) row$dataset_id, character(1)),
    file_group_id = vapply(
      rows,
      function(row) row$file_group_id,
      character(1)
    ),
    position = vapply(rows, function(row) row$position, integer(1)),
    name = vapply(rows, function(row) row$variable$name, character(1)),
    label = vapply(rows, function(row) row$variable$label, character(1)),
    description = vapply(
      rows,
      function(row) row$variable$description,
      character(1)
    ),
    unit = vapply(rows, function(row) row$variable$unit, character(1)),
    calibration = vapply(
      rows,
      function(row) row$variable$calibration,
      character(1)
    ),
    type = vapply(rows, function(row) row$variable$type, character(1)),
    term = vapply(rows, function(row) row$variable$term, character(1)),
    term_name = vapply(
      rows,
      function(row) row$variable$term_name,
      character(1)
    ),
    primary = vapply(rows, function(row) row$variable$primary, logical(1)),
    factor_values = lapply(rows, function(row) {
      vapply(
        row$variable$factor_levels,
        function(level) level$value,
        character(1)
      )
    }),
    factor_labels = lapply(rows, function(row) {
      vapply(
        row$variable$factor_levels,
        function(level) level$label,
        character(1)
      )
    }),
    factor_descriptions = lapply(rows, function(row) {
      vapply(
        row$variable$factor_levels,
        function(level) level$description,
        character(1)
      )
    }),
    selection_origin = rep(variable_scope, length(rows))
  )
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
  origin <- switch(
    variable_scope,
    matched = "matched_term",
    all = "all_declared",
    selected = "selected_name"
  )
  rows <- unlist(
    lapply(records, function(record) {
      if (!identical(record$status, "included")) {
        return(list())
      }
      selected_names <- vapply(
        record$selected_variables,
        function(variable) variable$name,
        character(1)
      )
      selected_rows <- lapply(
        seq_along(record$selected_variables),
        function(position) {
          variable <- record$selected_variables[[position]]
          list(
            unit_id = record$unit_id,
            dataset_id = record$dataset_id,
            file_group_id = record$file_group_id,
            position = as.integer(position),
            name = variable$name,
            declared_type = variable$type,
            origin = origin,
            selected_for_output = TRUE,
            automatic = FALSE,
            message = "Selected declared source column."
          )
        }
      )
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
      automatic_rows <- lapply(seq_along(datetime_columns), function(index) {
        name <- datetime_columns[[index]]
        variable <- record$variables[[match(name, declared_names)]]
        list(
          unit_id = record$unit_id,
          dataset_id = record$dataset_id,
          file_group_id = record$file_group_id,
          position = as.integer(length(selected_rows) + index),
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
      })
      c(selected_rows, automatic_rows)
    }),
    recursive = FALSE
  )
  if (length(rows) == 0L) {
    return(glc_plan_empty_read_columns())
  }
  tibble::tibble(
    unit_id = vapply(rows, function(row) row$unit_id, character(1)),
    dataset_id = vapply(rows, function(row) row$dataset_id, character(1)),
    file_group_id = vapply(
      rows,
      function(row) row$file_group_id,
      character(1)
    ),
    position = vapply(rows, function(row) row$position, integer(1)),
    name = vapply(rows, function(row) row$name, character(1)),
    declared_type = vapply(
      rows,
      function(row) row$declared_type,
      character(1)
    ),
    origin = vapply(rows, function(row) row$origin, character(1)),
    selected_for_output = vapply(
      rows,
      function(row) row$selected_for_output,
      logical(1)
    ),
    automatic = vapply(rows, function(row) row$automatic, logical(1)),
    message = vapply(rows, function(row) row$message, character(1))
  )
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
    factor_descriptions = list(),
    harmonization_required = logical(),
    harmonized_variables = list(),
    diagnostic_ids = list(),
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
      factor_descriptions = list(lapply(
        contract$variables,
        function(variable) variable$factor_descriptions
      )),
      harmonization_required = unit$harmonization_required,
      harmonized_variables = list(unit$harmonized_variables),
      diagnostic_ids = list(unit$diagnostic_ids),
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

glc_plan_empty_compatibility_sets <- function() {
  tibble::tibble(
    compatibility_id = character(),
    dataset_count = integer(),
    file_group_count = integer(),
    variable_count = integer(),
    file_count = integer(),
    declared_bytes = numeric(),
    known_file_count = integer(),
    unknown_file_count = integer(),
    declared_bytes_complete = logical(),
    harmonization_required = logical(),
    harmonized_variables = list(),
    diagnostic_ids = list(),
    final_unit_count = integer(),
    final_selection_required = logical(),
    constraint_codes = list(),
    constraint_messages = list(),
    file_group_ids = list(),
    final_unit_ids = list(),
    selected_names = list(),
    declared_types = list(),
    factor_values = list(),
    factor_labels = list(),
    factor_descriptions = list(),
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
    standardize_affects_structure = logical()
  )
}

glc_plan_compatibility_sets_table <- function(compatibility_sets) {
  if (length(compatibility_sets) == 0L) {
    return(glc_plan_empty_compatibility_sets())
  }
  dplyr::bind_rows(lapply(compatibility_sets, function(set) {
    contract <- set$compatibility
    tibble::tibble(
      compatibility_id = set$compatibility_id,
      dataset_count = set$dataset_count,
      file_group_count = set$file_group_count,
      variable_count = set$variable_count,
      file_count = set$file_count,
      declared_bytes = set$declared_bytes,
      known_file_count = set$known_file_count,
      unknown_file_count = set$unknown_file_count,
      declared_bytes_complete = set$declared_bytes_complete,
      harmonization_required = set$harmonization_required,
      harmonized_variables = list(set$harmonized_variables),
      diagnostic_ids = list(set$diagnostic_ids),
      final_unit_count = set$final_unit_count,
      final_selection_required = set$final_selection_required,
      constraint_codes = list(set$constraint_codes),
      constraint_messages = list(set$constraint_messages),
      file_group_ids = list(set$file_group_ids),
      final_unit_ids = list(set$final_unit_ids),
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
      factor_descriptions = list(lapply(
        contract$variables,
        function(variable) variable$factor_descriptions
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
      standardize_affects_structure = FALSE
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
#' Build a deterministic plan from validated declarations and normalized core
#' metadata. The plan separates structural compatibility sets from final
#' collectable units, and it never reads or inspects measurement contents.
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
#' @section Structural compatibility and final units:
#' Included groups are first compared by selected source names and declaration
#' order, declared types, time zone, ordered modalities, role, data state, and
#' datetime contract. Collection-based datetime values are record-specific and
#' are ignored when comparing otherwise identical collection-based contracts.
#'
#' Unordered factor declarations can share a structural set when their raw
#' values have compatible effective labels and descriptions and their declared
#' order constraints form an acyclic graph. An absent label means the raw value;
#' no text normalization or semantic inference is performed. The union order is
#' deterministic and preserves every declared order constraint. Duplicate raw
#' values, ambiguous labels, conflicting labels or non-missing descriptions,
#' cyclic order constraints, and unsupported ordered factors remain blocking.
#' Blocking families retain exact factor-contract partitions.
#'
#' Each structural set has a `compatibility_id` beginning with `"glcc_"`. It is
#' a version 2 SHA-256 digest of length-prefixed canonical UTF-8 text containing
#' the exact package identity, repository, source revision, package schema, and
#' the full structural-family contract computed before `dataset_id` and
#' `file_group` restrictions are applied. It does not contain current membership,
#' restrictions, metadata facets, or `standardize`. The identifier therefore
#' stays the same when an unchanged family is narrowed at the same package
#' revision. Wearing position, device identity or location, participant
#' characteristics, file description, site context, and other descriptive
#' metadata do not split structural sets.
#'
#' Final units enforce consistent stable file-group relationships and dataset
#' study and participant relationships. Device identity is file-group-scoped,
#' so distinct file groups in one dataset may reference different devices while
#' remaining in one final unit. The active factor union is recomputed after
#' restrictions. [glc_read()] carries the raw declaration contract and
#' [glc_collect()] validates and applies the same union to actual factors.
#'
#' A `unit_id` begins with `"glcu_"` and uses version 2 canonical identity. It
#' hashes the package and revision, normalized request including restrictions
#' and `standardize`, active structural contract, and sorted member file-group
#' identifiers. Final unit ids are request- and membership-sensitive, while
#' structural ids are restriction stable. Both use canonical UTF-8 text rather
#' than R serialized-object bytes, and their values and table order are stable
#' under input row reordering. Version 2 identifiers deliberately differ from
#' earlier identifiers because factor equivalence and device allocation rules
#' changed.
#'
#' `compatibility_diagnostics` explains safe unions and blocking differences.
#' Selected-variable diagnostics describe the current partition. Non-selected
#' diagnostics disclose what would require harmonization or block a later
#' expanded variable request without selecting those variables now. Re-plan an
#' expanded request before reading or collecting additional variables.
#'
#' @section Declaration-only assurance:
#' The planner uses the validated descriptor and core metadata associated with
#' `x`. One explicit planning call may load descriptor-declared resources named
#' `study`, `participants`, `participant_characteristics`, `datasets`,
#' `devices`, `device_datasheets`, and optional `contributors` at the exact
#' source revision. For a remote package, loading an uncached core resource can
#' make an HTTP request. The allowlist is exactly the value returned internally
#' by `glc_core_resource_names()`.
#'
#' The planner does not call [glc_read()], [glc_collect()], [glc_files()],
#' [glc_summary()], or [glc_download()]. It never requests a measurement path,
#' probes measurement availability, or inspects source rows. Known
#' declaration-level constraints, including reserved source names that begin
#' with `.glc_`, are applied before units are formed.
#'
#' File sizes come only from an explicit supported byte declaration or an entry
#' already present in the local manifest. The planner never downloads a file or
#' probes a remote object to discover its size; unavailable sizes remain `NA`.
#' A unit's `declared_bytes` is the sum of known sizes, and
#' `declared_bytes_complete` records whether every file size is known.
#'
#' Compatibility is an assurance about validated declarations, not downloaded
#' values. Actual columns, parsed classes and factor values, datetime values, and
#' output-column collisions can only be checked after reading. [glc_read()]
#' remains authoritative for source-file validation. [glc_collect()] validates
#' the preserved raw factor declarations, harmonizes only safe unions, and
#' remains authoritative for final compatibility and standardization.
#'
#' @section Missing values and relationship links:
#' Source missingness is retained. Missing scalar metadata stays as a typed
#' `NA`, and absent repeated metadata stays an empty vector or list. Literal
#' source values such as `"not applicable"` remain literal values. The planner
#' does not synthesize a description, instrument, contributor id, or site id.
#'
#' Relationship status columns use `"linked"`, `"not_applicable"`,
#' `"unresolved"`, or `"metadata_unavailable"`. `"not_applicable"` means that
#' no link applies, such as a dataset not associated with a participant or a
#' group without a device id. `"unresolved"` means that an applicable id is
#' missing or does not resolve in loaded metadata. `"metadata_unavailable"`
#' means that an id is present but its optional core resource was not declared.
#'
#' @section Recommended interactive workflow:
#' Build one plan as an explicit planning task. Present one reader-oriented
#' option per row of `compatibility_sets`, then filter `groups` and the
#' normalized `metadata` tables in memory by stable ids. Do not rebuild the plan
#' for each participant, device, position, characteristic, or site filter.
#' Finally pass the narrowed file-group ids to [glc_collection_refine()]. A
#' caller should proceed to reading only when the refinement reports one final
#' unit and `final_selection_required = FALSE`. Keep the parent plan because the
#' lightweight refinement does not copy its metadata tables.
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
#'   measurement transfer and inspection flags, `core_metadata_transport`,
#'   interactive-filter and refinement network flags, and `byte_policy`.
#' * `preferred_unit_id`: the preferred unit, or `NA_character_` when no unit is
#'   collectable. Preference is deterministic: most datasets, then most file
#'   groups, then the lexically smallest unit id.
#' * `units`: one row per final unit. Columns are `unit_id`,
#'   `compatibility_id`, `preferred`, `dataset_count`, `file_group_count`,
#'   `variable_count`, `file_count`, `declared_bytes`, `known_file_count`,
#'   `unknown_file_count`, `declared_bytes_complete`, `harmonization_required`,
#'   and list-columns `harmonized_variables`, `diagnostic_ids`, and
#'   `file_group_ids`.
#' * `groups`: one row per declared file group. Columns are `status`, `unit_id`,
#'   `compatibility_id`, `dataset_id`, integer declaration index `file_group`,
#'   stable `file_group_id`, `study_id`, `participant_id`,
#'   `participant_associated`, `study_link_status`, `participant_link_status`,
#'   `device_id`, `device_link_status`, `datasheet_id`, `device_location`,
#'   `device_location_type`, `description`, `instructions`,
#'   `instrument_declared`, `dataset_timezone`, `dataset_latitude`,
#'   `dataset_longitude`, `format`, `timezone`, list-column `modalities`,
#'   `modality_other`, `modality_other_type`, `role`, `data_state`,
#'   `temporal_type`, `temporal_value`, `temporal_unit`, `header_row`,
#'   list-column `preprocessing`, `datetime_source`, `datetime_date`,
#'   `datetime_format`, `datetime_time`, `datetime_time_format`, and list-columns
#'   `selected_variables`, `reason_codes`, and `messages`. Excluded groups have
#'   missing `unit_id` and `compatibility_id` values.
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
#'   `selected_names`, `declared_types`, `factor_values`, `factor_labels`, and
#'   `factor_descriptions`, `harmonization_required`, list-columns
#'   `harmonized_variables` and `diagnostic_ids`, `timezone`, list-column
#'   `modalities`, `role`, `data_state`,
#'   `datetime_source`, `datetime_signature`, `datetime_date`,
#'   `datetime_format`, `datetime_time`, `datetime_time_format`,
#'   `collection_values_ignored`, `relationship_rule`, `device_rule`,
#'   `standardize`, and the fixed `standardize_affects_partition = FALSE`
#'   assurance.
#' * `extensions`: one row per group. Columns are `dataset_id`,
#'   `file_group_id`, and preserved, forward-compatible unknown declaration
#'   fields in the plain list-column `metadata`.
#' * `compatibility_sets`: one row per structural set. Columns are
#'   `compatibility_id`, dataset, file-group, variable, and file counts; byte
#'   summaries; `harmonization_required`; list-columns `harmonized_variables`
#'   and `diagnostic_ids`; `final_unit_count`; `final_selection_required`;
#'   list-columns `constraint_codes`, `constraint_messages`, `file_group_ids`, and
#'   `final_unit_ids`; the selected-name, declared-type, factor, timezone,
#'   modality, role, data-state, and datetime contract columns also present in
#'   `compatibility`; `relationship_rule`; `device_rule`; and the fixed
#'   `standardize_affects_structure = FALSE` assurance.
#' * `compatibility_diagnostics`: one row per stable diagnostic. Columns are
#'   `diagnostic_id`, `selection_scope`, `classification`, `code`,
#'   `variable_name`, `applies_to_current_plan`, list-column
#'   `prospective_scopes`, `message`, affected group, structure, and unit counts,
#'   and list-columns `file_group_ids`, `compatibility_ids`, `unit_ids`,
#'   `union_values`, `union_labels`, and `union_descriptions`.
#' * `compatibility_diagnostic_groups`: one row per affected diagnostic and
#'   file-group pair. Columns are `diagnostic_id`, `dataset_id`, stable
#'   `file_group_id`, `current_status`, `compatibility_id`, `unit_id`,
#'   `variable_present`, `selected_by_request`, `declaration_position`,
#'   `declared_type`, and factor value, label, and description list-columns.
#' * `metadata`: a normalized typed core-metadata snapshot described below.
#' * `refinement_input`: a compact, serializable input used and validated by
#'   [glc_collection_refine()]. It contains `schema`, `version`,
#'   `canonicalization`, `digest_algorithm`, compact `provenance` and `request`
#'   lists, a `membership` table, plain-list structural `contracts`, per-group
#'   declarations and relationship facts, and a SHA-256 `fingerprint`.
#'   Consumers should not modify or reconstruct it.
#'
#' All tables are tibbles with stable columns, including when they have no rows.
#' List-columns contain only plain serializable vectors and lists. `print()`
#' shows a compact package, request, unit, diagnostic, harmonization, byte,
#' preferred-unit, and assurance summary and returns the plan invisibly.
#'
#' @section Normalized metadata tables:
#' `metadata` has schema `"glc-package-metadata"`, version `"1.0.0"`, and the
#' following stable tables. All identifier joins are explicit; there is no
#' `sites` table because the supported source schema has no stable site id.
#'
#' * `resource_status`: `resource`, `declared`, `status`, and `record_count`.
#'   Status is `"not_declared"`, `"loaded_empty"`, or `"loaded"`.
#' * `studies`: `study_id`, `schema_version`, `title`, `short_description`,
#'   `preregistration`, `registration`, `ethics`, `sample`, `intervention`,
#'   `setting`, `geographical_location`, `study_type`, and list-columns
#'   `funding_sources`, `keywords`, and `dataset_ids`.
#' * `study_groups`: `study_id`, `position`, `name`, `description`, `size`, and
#'   list-columns `inclusion`, `exclusion`, and `dataset_ids`.
#' * `study_contributors`: `study_id`, `position`, `full_name`, list-column
#'   `roles`, `email`, `orcid`, `institution_name`, `institution_city`, and
#'   `institution_country`.
#' * `contributors`: `contributor_id`, deterministic row key `position`,
#'   `full_name`, list-column `roles`, `email`, `orcid`, `institution_name`,
#'   `institution_city`, and `institution_country`. A missing source id remains
#'   typed `NA`; no id is synthesized.
#' * `datasets`: `dataset_id`, `schema_version`, `study_id`,
#'   `study_link_status`, `participant_id`, `participant_associated`,
#'   `participant_link_status`, `timezone`, numeric `latitude` and `longitude`,
#'   `file_group_count`, `file_count`, and list-columns `modalities`,
#'   `device_ids`, and `primary_variables`.
#' * `dataset_terms`: `dataset_id`, `position`, canonical `term`, and `label`.
#' * `participants`: `participant_id`, numeric `age`, `sex`, and `gender`.
#' * `participant_characteristics`: `participant_id`,
#'   `participant_link_status`, `characteristic_position`, `value_position`,
#'   `name`, typed scalar list-column `value`, `value_type`, `unit`, and
#'   `description`.
#' * `devices`: `device_id`, `schema_version`, `manufacturer`, `model`,
#'   `serial_number`, `calibration_date`, `firmware_version`, `datasheet_id`, and
#'   `datasheet_link_status`.
#' * `device_sensors`: `device_id`, `position`, `sensor_type`, `datasheet_id`,
#'   and `datasheet_link_status`.
#' * `datasheets`: `datasheet_id`, `schema_version`, `datasheet_version`,
#'   `manufacturer`, `type`, list-column `modalities`, `modality_other`, `model`,
#'   `calibration_interval`, `calibration_method`, `calibration_accuracy`,
#'   `calibration_range`, `calibration_notes`, typed list-column
#'   `calibration_spectral_sensitivity`, `calibration_linearity`, and
#'   `calibration_directional_response`.
#' * `datasheet_parameters`: `datasheet_id`, `position`, `name`, typed scalar
#'   list-column `value`, `value_type`, `unit`, and `description`.
#' * `datasheet_channels`: `datasheet_id`, `position`, integer `channel_number`,
#'   `name`, `description`, and `unit`.
#' * `instruments`: `dataset_id`, `file_group_id`, `instrument_type`,
#'   `instrument_name`, `collection_method`, `recorded_by`, and `software_name`.
#' * `file_group_variables`: all declared variables for every included group,
#'   independent of `variable_scope`. Columns are `dataset_id`, `file_group_id`,
#'   `declaration_position`, `selected_by_request`, `selected_for_output`,
#'   `selection_origin`, `name`, `label`, `description`, `unit`, `calibration`,
#'   `type`, canonical `term`, `term_name`, `primary`, and `factor_level_count`.
#' * `file_group_factor_levels`: `dataset_id`, `file_group_id`,
#'   `variable_position`, `variable_name`, `level_position`, `value`, `label`,
#'   and `description`.
#' * `extensions`: `resource`, `entity_type`, `entity_id`, `parent_id`,
#'   `position`, and plain list-column `metadata` for forward-compatible unknown
#'   fields. Standard fields never require parsing this column.
#'
#' @section Exclusions and errors:
#' Per-group declaration outcomes are returned rather than thrown. Stable reason
#' codes are `included`, `scope_dataset`, `scope_file_group`,
#' `reserved_provenance_column`, `term_missing`, `variable_missing`,
#' `invalid_factor_contract`, `no_declared_files`, `unsupported_format`,
#' `invalid_timezone`, and `incomplete_datetime`; each has a plain-language
#' message.
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
#' @return A serializable `glc_collection_plan` object using plan schema
#'   `"glc-collection-plan"` version `"1.2.0"`, as described in **Return
#'   tables**.
#' @seealso [glc_collection_refine()] for fast in-memory narrowing,
#'   [glc_variables()] for declared selectors, [glc_read()] for runtime import
#'   validation, and [glc_collect()] for authoritative final collection.
#' @export
#'
#' @examples
#' \dontrun{
#' # Use an existing local, manifest-backed package directory.
#' # This pattern performs no network request and does not read measurements.
#' pkg <- glc_open("path/to/manifest-backed-package", quiet = TRUE)
#' plan <- glc_collection_plan(
#'   pkg,
#'   terms = "photopic illuminance",
#'   variable_scope = "matched"
#' )
#' plan$compatibility_sets[, c(
#'   "compatibility_id", "file_group_count", "final_unit_count",
#'   "final_selection_required"
#' )]
#'
#' # Filter plan$groups and plan$metadata in memory, then refine exact ids.
#' set_id <- plan$compatibility_sets$compatibility_id[[1L]]
#' selected_ids <- plan$compatibility_sets$file_group_ids[[1L]]
#' refined <- glc_collection_refine(plan, selected_ids, set_id)
#' refined
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
  model <- glc_model(x)
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
  metadata_snapshot <- glc_plan_metadata_snapshot(
    x,
    model,
    engine$records,
    variable_scope
  )
  records <- metadata_snapshot$records
  engine$records <- records
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
  refinement_input <- glc_plan_refinement_input(
    records,
    request,
    provenance
  )
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
        core_metadata_transport = paste0(
          "Only descriptor-declared core resources at the exact source ",
          "revision may be loaded during this explicit planning call."
        ),
        interactive_filtering_network_access = FALSE,
        refinement_network_access = FALSE,
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
      extensions = glc_plan_extensions_table(records),
      compatibility_sets = glc_plan_compatibility_sets_table(
        engine$compatibility_sets
      ),
      compatibility_diagnostics = glc_plan_compatibility_diagnostics_table(
        engine$diagnostics,
        records
      ),
      compatibility_diagnostic_groups = glc_plan_compatibility_diagnostic_groups_table(
        engine$diagnostics,
        records
      ),
      metadata = metadata_snapshot$metadata,
      refinement_input = refinement_input
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
    " final unit(s) in ",
    nrow(x$compatibility_sets),
    " structural set(s); ",
    included,
    " included, ",
    excluded,
    " excluded group(s)\n",
    sep = ""
  )
  safe_diagnostics <- sum(
    x$compatibility_diagnostics$classification == "safely_harmonizable"
  )
  blocking_diagnostics <- sum(
    x$compatibility_diagnostics$classification == "blocking"
  )
  harmonized_structures <- sum(
    x$compatibility_sets$harmonization_required
  )
  cat(
    "Compatibility: ",
    safe_diagnostics,
    " safely harmonizable, ",
    blocking_diagnostics,
    " blocking diagnostic(s); ",
    harmonized_structures,
    " active harmonized structure(s)\n",
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
