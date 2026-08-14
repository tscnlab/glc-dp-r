glc_factor_contract_schema <- function() {
  "glc-factor-contract"
}

glc_factor_contract_version <- function() {
  "1.0.0"
}

glc_factor_effective_labels <- function(values, labels) {
  labels <- as.character(labels)
  missing <- is.na(labels) | !nzchar(labels)
  labels[missing] <- values[missing]
  enc2utf8(labels)
}

glc_factor_effective_descriptions <- function(descriptions, size) {
  descriptions <- as.character(descriptions)
  if (length(descriptions) == 0L) {
    descriptions <- rep(NA_character_, size)
  }
  descriptions[!is.na(descriptions) & !nzchar(descriptions)] <- NA_character_
  enc2utf8(descriptions)
}

glc_factor_contract_variable <- function(variable) {
  levels <- variable$factor_levels %||% list()
  values <- vapply(levels, function(level) level$value, character(1))
  labels <- vapply(levels, function(level) level$label, character(1))
  descriptions <- vapply(
    levels,
    function(level) level$description,
    character(1)
  )
  list(
    name = enc2utf8(variable$name),
    type = enc2utf8(variable$type),
    ordered = isTRUE(variable$ordered),
    factor_values = enc2utf8(values),
    factor_labels = glc_factor_effective_labels(values, labels),
    factor_descriptions = glc_factor_effective_descriptions(
      descriptions,
      length(values)
    )
  )
}

glc_factor_contract_issue <- function(variable) {
  valid_header <- is.list(variable) &&
    is.character(variable$name) &&
    length(variable$name) == 1L &&
    !is.na(variable$name) &&
    nzchar(variable$name) &&
    is.character(variable$type) &&
    length(variable$type) == 1L &&
    !is.na(variable$type) &&
    is.logical(variable$ordered) &&
    length(variable$ordered) == 1L &&
    !is.na(variable$ordered) &&
    is.character(variable$factor_values) &&
    is.character(variable$factor_labels) &&
    is.character(variable$factor_descriptions) &&
    length(variable$factor_values) == length(variable$factor_labels) &&
    length(variable$factor_values) == length(variable$factor_descriptions)
  if (!valid_header) {
    return(list(
      code = "factor_contract_incomplete",
      message = "A variable contains an incomplete declared factor contract."
    ))
  }
  if (!identical(variable$type, "factor")) {
    if (
      length(variable$factor_values) > 0L ||
        length(variable$factor_labels) > 0L ||
        length(variable$factor_descriptions) > 0L ||
        isTRUE(variable$ordered)
    ) {
      return(list(
        code = "factor_contract_incomplete",
        message = paste0(
          "Non-factor variable ",
          encodeString(variable$name, quote = "\""),
          " contains factor-only declaration fields."
        )
      ))
    }
    return(NULL)
  }
  if (isTRUE(variable$ordered)) {
    return(list(
      code = "ordered_factor_unsupported",
      message = paste0(
        "Variable ",
        encodeString(variable$name, quote = "\""),
        " declares an ordered factor, which this schema does not support."
      )
    ))
  }
  values <- variable$factor_values
  labels <- glc_factor_effective_labels(values, variable$factor_labels)
  if (length(values) == 0L || anyNA(values) || any(!nzchar(values))) {
    return(list(
      code = "factor_value_missing",
      message = paste0(
        "Factor variable ",
        encodeString(variable$name, quote = "\""),
        " must declare non-empty factor values."
      )
    ))
  }
  if (anyDuplicated(values)) {
    return(list(
      code = "factor_value_duplicate",
      message = paste0(
        "Factor variable ",
        encodeString(variable$name, quote = "\""),
        " declares duplicate raw values."
      )
    ))
  }
  duplicated_labels <- unique(labels[duplicated(labels)])
  if (length(duplicated_labels) > 0L) {
    return(list(
      code = "factor_label_ambiguous",
      message = paste0(
        "Factor variable ",
        encodeString(variable$name, quote = "\""),
        " maps different raw values to the same effective label: ",
        paste(duplicated_labels, collapse = ", "),
        "."
      )
    ))
  }
  NULL
}

glc_factor_contract_key <- function(variable) {
  glc_plan_canonical_text(list(
    values = variable$factor_values,
    labels = variable$factor_labels,
    descriptions = variable$factor_descriptions,
    ordered = variable$ordered
  ))
}

glc_factor_union_failure <- function(code, message, variable_name) {
  list(
    compatible = FALSE,
    code = code,
    message = message,
    variable_name = variable_name,
    harmonization_required = FALSE,
    variable = NULL
  )
}

glc_factor_union_order <- function(contracts) {
  values <- unique(unlist(lapply(
    contracts,
    function(contract) contract$factor_values
  )))
  values <- glc_plan_sort_utf8(values)
  adjacency <- stats::setNames(vector("list", length(values)), values)
  indegree <- stats::setNames(rep.int(0L, length(values)), values)
  minimum_position <- stats::setNames(
    rep.int(.Machine$integer.max, length(values)),
    values
  )

  for (contract in contracts) {
    contract_values <- contract$factor_values
    for (position in seq_along(contract_values)) {
      value <- contract_values[[position]]
      minimum_position[[value]] <- min(minimum_position[[value]], position)
    }
    if (length(contract_values) < 2L) {
      next
    }
    for (position in seq_len(length(contract_values) - 1L)) {
      from <- contract_values[[position]]
      to <- contract_values[[position + 1L]]
      if (!to %in% adjacency[[from]]) {
        adjacency[[from]] <- c(adjacency[[from]], to)
        indegree[[to]] <- indegree[[to]] + 1L
      }
    }
  }

  result <- character()
  remaining <- values
  while (length(remaining) > 0L) {
    available <- remaining[indegree[remaining] == 0L]
    if (length(available) == 0L) {
      return(NULL)
    }
    next_value <- available[order(
      minimum_position[available],
      glc_plan_utf8_key(available),
      method = "radix"
    )][[1L]]
    result <- c(result, next_value)
    remaining <- setdiff(remaining, next_value)
    for (to in adjacency[[next_value]]) {
      indegree[[to]] <- indegree[[to]] - 1L
    }
  }
  result
}

glc_factor_union <- function(contracts) {
  contracts <- lapply(contracts, function(contract) {
    contract$factor_labels <- glc_factor_effective_labels(
      contract$factor_values,
      contract$factor_labels
    )
    contract$factor_descriptions <- glc_factor_effective_descriptions(
      contract$factor_descriptions,
      length(contract$factor_values)
    )
    contract
  })
  variable_name <- contracts[[1L]]$name
  issues <- lapply(contracts, glc_factor_contract_issue)
  invalid <- which(!vapply(issues, is.null, logical(1)))
  if (length(invalid) > 0L) {
    issue <- issues[[invalid[[1L]]]]
    return(glc_factor_union_failure(
      issue$code,
      issue$message,
      variable_name
    ))
  }

  values <- unique(unlist(lapply(
    contracts,
    function(contract) contract$factor_values
  )))
  labels_by_value <- stats::setNames(vector("list", length(values)), values)
  descriptions_by_value <- stats::setNames(
    vector("list", length(values)),
    values
  )
  for (contract in contracts) {
    for (position in seq_along(contract$factor_values)) {
      value <- contract$factor_values[[position]]
      labels_by_value[[value]] <- unique(c(
        labels_by_value[[value]],
        contract$factor_labels[[position]]
      ))
      description <- contract$factor_descriptions[[position]]
      if (!is.na(description)) {
        descriptions_by_value[[value]] <- unique(c(
          descriptions_by_value[[value]],
          description
        ))
      }
    }
  }

  label_conflicts <- names(labels_by_value)[
    vapply(
      labels_by_value,
      length,
      integer(1)
    ) >
      1L
  ]
  if (length(label_conflicts) > 0L) {
    value <- glc_plan_sort_utf8(label_conflicts)[[1L]]
    return(glc_factor_union_failure(
      "factor_label_conflict",
      paste0(
        "Variable ",
        encodeString(variable_name, quote = "\""),
        " maps raw value ",
        encodeString(value, quote = "\""),
        " to conflicting labels."
      ),
      variable_name
    ))
  }

  labels <- vapply(labels_by_value, function(value) value[[1L]], character(1))
  ambiguous <- unique(labels[duplicated(labels)])
  if (length(ambiguous) > 0L) {
    return(glc_factor_union_failure(
      "factor_label_ambiguous",
      paste0(
        "Variable ",
        encodeString(variable_name, quote = "\""),
        " maps different raw values to the same effective label: ",
        paste(glc_plan_sort_utf8(ambiguous), collapse = ", "),
        "."
      ),
      variable_name
    ))
  }

  description_conflicts <- names(descriptions_by_value)[
    vapply(
      descriptions_by_value,
      length,
      integer(1)
    ) >
      1L
  ]
  if (length(description_conflicts) > 0L) {
    value <- glc_plan_sort_utf8(description_conflicts)[[1L]]
    return(glc_factor_union_failure(
      "factor_description_conflict",
      paste0(
        "Variable ",
        encodeString(variable_name, quote = "\""),
        " gives raw value ",
        encodeString(value, quote = "\""),
        " conflicting non-missing descriptions."
      ),
      variable_name
    ))
  }

  order <- glc_factor_union_order(contracts)
  if (is.null(order)) {
    return(glc_factor_union_failure(
      "factor_order_conflict",
      paste0(
        "Variable ",
        encodeString(variable_name, quote = "\""),
        " has incompatible factor-level order constraints."
      ),
      variable_name
    ))
  }
  union_labels <- unname(vapply(
    order,
    function(value) {
      labels_by_value[[value]][[1L]]
    },
    character(1)
  ))
  union_descriptions <- unname(vapply(
    order,
    function(value) {
      descriptions <- descriptions_by_value[[value]]
      if (length(descriptions) == 0L) NA_character_ else descriptions[[1L]]
    },
    character(1)
  ))
  keys <- unique(vapply(contracts, glc_factor_contract_key, character(1)))
  variable <- contracts[[1L]]
  variable$factor_values <- order
  variable$factor_labels <- union_labels
  variable$factor_descriptions <- union_descriptions
  list(
    compatible = TRUE,
    code = if (length(keys) > 1L) "factor_level_union" else NA_character_,
    message = if (length(keys) > 1L) {
      paste0(
        "Variable ",
        encodeString(variable_name, quote = "\""),
        " has compatible factor declarations. Levels will be harmonized to: ",
        paste(order, collapse = ", "),
        "."
      )
    } else {
      NA_character_
    },
    variable_name = variable_name,
    harmonization_required = length(keys) > 1L,
    variable = variable
  )
}

glc_compatibility_base_contract <- function(contract) {
  value <- contract
  value$variables <- lapply(value$variables, function(variable) {
    list(
      name = variable$name,
      type = variable$type,
      ordered = isTRUE(variable$ordered)
    )
  })
  value
}

glc_compatibility_context_contract <- function(contract) {
  value <- contract
  value$variables <- NULL
  value
}

glc_compatibility_merge_contracts <- function(contracts) {
  base_keys <- unique(vapply(
    contracts,
    function(contract) {
      glc_plan_canonical_text(glc_compatibility_base_contract(contract))
    },
    character(1)
  ))
  if (length(base_keys) != 1L) {
    return(list(
      compatible = FALSE,
      contract = NULL,
      results = list()
    ))
  }
  merged <- contracts[[1L]]
  results <- vector("list", length(merged$variables))
  for (position in seq_along(merged$variables)) {
    variables <- lapply(contracts, function(contract) {
      contract$variables[[position]]
    })
    if (identical(variables[[1L]]$type, "factor")) {
      result <- glc_factor_union(variables)
      results[[position]] <- result
      if (isTRUE(result$compatible)) {
        merged$variables[[position]] <- result$variable
      }
    } else {
      results[[position]] <- list(
        compatible = TRUE,
        code = NA_character_,
        message = NA_character_,
        variable_name = variables[[1L]]$name,
        harmonization_required = FALSE,
        variable = variables[[1L]]
      )
    }
  }
  list(
    compatible = all(vapply(
      results,
      function(result) {
        isTRUE(result$compatible)
      },
      logical(1)
    )),
    contract = merged,
    results = results
  )
}

glc_runtime_factor_contract <- function(variables) {
  contract <- list(
    schema = glc_factor_contract_schema(),
    version = glc_factor_contract_version(),
    variables = lapply(variables, glc_factor_contract_variable)
  )
  c(contract, list(fingerprint = glc_plan_digest(contract)))
}

glc_validate_runtime_factor_contract <- function(contract) {
  valid_header <- is.list(contract) &&
    identical(contract$schema, glc_factor_contract_schema()) &&
    identical(contract$version, glc_factor_contract_version()) &&
    is.list(contract$variables) &&
    is.character(contract$fingerprint) &&
    length(contract$fingerprint) == 1L &&
    !is.na(contract$fingerprint) &&
    grepl("^[0-9a-f]{64}$", contract$fingerprint)
  if (!valid_header) {
    glc_abort(
      "The collection contains an incomplete factor-contract payload.",
      class = "glcdp_factor_contract_invalid"
    )
  }
  body <- contract[setdiff(names(contract), "fingerprint")]
  if (!identical(glc_plan_digest(body), contract$fingerprint)) {
    glc_abort(
      "The collection factor-contract payload has changed since reading.",
      class = "glcdp_factor_contract_tampered"
    )
  }
  issues <- lapply(contract$variables, glc_factor_contract_issue)
  invalid <- which(!vapply(issues, is.null, logical(1)))
  if (length(invalid) > 0L) {
    glc_abort(
      issues[[invalid[[1L]]]]$message,
      class = "glcdp_factor_contract_invalid"
    )
  }
  contract
}

glc_compatibility_diagnostic_schema <- function() {
  "glc-collection-compatibility-diagnostic"
}

glc_compatibility_diagnostic_version <- function() {
  "1.0.0"
}

glc_compatibility_diagnostic_id <- function(
  code,
  variable_name,
  selection_scope,
  records,
  provenance
) {
  record_ids <- vapply(
    records,
    function(record) {
      record$file_group_id
    },
    character(1)
  )
  records <- records[order(glc_plan_utf8_key(record_ids), method = "radix")]
  material <- list(
    schema = glc_compatibility_diagnostic_schema(),
    version = glc_compatibility_diagnostic_version(),
    package = provenance[c(
      "package_id",
      "repository",
      "source_revision",
      "package_schema_version"
    )],
    code = code,
    variable_name = variable_name,
    selection_scope = selection_scope,
    declarations = lapply(records, function(record) {
      variable_names <- vapply(
        record$variables,
        function(variable) variable$name,
        character(1)
      )
      variable_position <- match(variable_name, variable_names)
      declaration <- if (!is.na(variable_position)) {
        glc_factor_contract_variable(record$variables[[variable_position]])
      } else {
        lapply(record$selected_variables, glc_factor_contract_variable)
      }
      list(
        file_group_id = record$file_group_id,
        declaration = declaration
      )
    })
  )
  paste0("glcd_", glc_plan_digest(material))
}

glc_new_compatibility_diagnostic <- function(
  code,
  classification,
  variable_name,
  message,
  selection_scope,
  records,
  provenance,
  union_variable = NULL,
  prospective_scopes = character()
) {
  if (identical(selection_scope, "not_selected")) {
    message <- paste0(
      message,
      " This variable is not selected in the current plan. Re-plan with an ",
      "expanded variable request before reading or collecting it."
    )
  }
  list(
    diagnostic_id = glc_compatibility_diagnostic_id(
      code,
      variable_name,
      selection_scope,
      records,
      provenance
    ),
    selection_scope = selection_scope,
    classification = classification,
    code = code,
    variable_name = variable_name,
    prospective_scopes = prospective_scopes,
    message = message,
    affected_file_group_ids = glc_plan_sort_utf8(vapply(
      records,
      function(record) record$file_group_id,
      character(1)
    )),
    union_values = union_variable$factor_values %||% character(),
    union_labels = union_variable$factor_labels %||% character(),
    union_descriptions = union_variable$factor_descriptions %||% character()
  )
}

glc_selected_contract_diagnostics <- function(
  records,
  request,
  provenance
) {
  if (length(records) < 2L) {
    return(list())
  }
  diagnostics <- list()
  contracts <- lapply(records, function(record) record$declared_compatibility)
  names_by_record <- lapply(contracts, function(contract) {
    vapply(contract$variables, function(variable) variable$name, character(1))
  })
  name_keys <- vapply(names_by_record, glc_plan_canonical_text, character(1))
  if (length(unique(name_keys)) > 1L) {
    diagnostics[[length(diagnostics) + 1L]] <-
      glc_new_compatibility_diagnostic(
        code = "declared_column_set_conflict",
        classification = "blocking",
        variable_name = NA_character_,
        message = paste0(
          "The affected file groups declare different selected source names ",
          "or declaration orders."
        ),
        selection_scope = "selected",
        records = records,
        provenance = provenance,
        prospective_scopes = request$variable_scope
      )
    return(diagnostics)
  }
  for (position in seq_along(contracts[[1L]]$variables)) {
    variables <- lapply(contracts, function(contract) {
      contract$variables[[position]]
    })
    types <- unique(vapply(
      variables,
      function(variable) {
        variable$type
      },
      character(1)
    ))
    if (length(types) > 1L) {
      name <- variables[[1L]]$name
      diagnostics[[length(diagnostics) + 1L]] <-
        glc_new_compatibility_diagnostic(
          code = "variable_type_conflict",
          classification = "blocking",
          variable_name = name,
          message = paste0(
            "Variable ",
            encodeString(name, quote = "\""),
            " has conflicting declared types across the affected file groups."
          ),
          selection_scope = "selected",
          records = records,
          provenance = provenance,
          prospective_scopes = request$variable_scope
        )
    }
  }
  diagnostics
}

glc_factor_result_diagnostic <- function(
  result,
  selection_scope,
  records,
  provenance,
  prospective_scopes
) {
  if (is.na(result$code)) {
    return(NULL)
  }
  glc_new_compatibility_diagnostic(
    code = result$code,
    classification = if (isTRUE(result$compatible)) {
      "safely_harmonizable"
    } else {
      "blocking"
    },
    variable_name = result$variable_name,
    message = result$message,
    selection_scope = selection_scope,
    records = records,
    provenance = provenance,
    union_variable = result$variable,
    prospective_scopes = prospective_scopes
  )
}

glc_nonselected_contract_diagnostics <- function(
  records,
  selected_names,
  provenance
) {
  if (length(records) < 2L) {
    return(list())
  }
  diagnostics <- list()
  names_by_record <- lapply(records, function(record) {
    vapply(record$variables, function(variable) variable$name, character(1))
  })
  full_name_keys <- vapply(
    names_by_record,
    glc_plan_canonical_text,
    character(1)
  )
  if (length(unique(full_name_keys)) > 1L) {
    diagnostics[[length(diagnostics) + 1L]] <-
      glc_new_compatibility_diagnostic(
        code = "declared_column_set_conflict",
        classification = "blocking",
        variable_name = NA_character_,
        message = paste0(
          "The affected file groups declare different complete source-column ",
          "sets or declaration orders."
        ),
        selection_scope = "not_selected",
        records = records,
        provenance = provenance,
        prospective_scopes = c("all", "selected")
      )
  }
  all_names <- glc_plan_sort_utf8(unique(unlist(names_by_record)))
  nonselected <- setdiff(all_names, selected_names)
  for (name in nonselected) {
    positions <- lapply(names_by_record, function(names) which(names == name))
    present <- lengths(positions) == 1L
    if (!all(present)) {
      diagnostics[[length(diagnostics) + 1L]] <-
        glc_new_compatibility_diagnostic(
          code = "variable_presence_conflict",
          classification = "blocking",
          variable_name = name,
          message = paste0(
            "Variable ",
            encodeString(name, quote = "\""),
            " is not declared by every affected file group."
          ),
          selection_scope = "not_selected",
          records = records,
          provenance = provenance,
          prospective_scopes = c("all", "selected")
        )
      next
    }
    variables <- lapply(seq_along(records), function(index) {
      glc_factor_contract_variable(
        records[[index]]$variables[[positions[[index]][[1L]]]]
      )
    })
    types <- unique(vapply(
      variables,
      function(variable) {
        variable$type
      },
      character(1)
    ))
    if (length(types) > 1L) {
      diagnostics[[length(diagnostics) + 1L]] <-
        glc_new_compatibility_diagnostic(
          code = "variable_type_conflict",
          classification = "blocking",
          variable_name = name,
          message = paste0(
            "Variable ",
            encodeString(name, quote = "\""),
            " has conflicting declared types across the affected file groups."
          ),
          selection_scope = "not_selected",
          records = records,
          provenance = provenance,
          prospective_scopes = c("all", "selected")
        )
      next
    }
    if (identical(types[[1L]], "factor")) {
      result <- glc_factor_union(variables)
      diagnostic <- glc_factor_result_diagnostic(
        result,
        "not_selected",
        records,
        provenance,
        c("all", "selected")
      )
      if (!is.null(diagnostic)) {
        diagnostics[[length(diagnostics) + 1L]] <- diagnostic
      }
    }
  }
  diagnostics
}

glc_collection_compatibility_families <- function(
  records,
  request,
  provenance
) {
  eligible <- which(vapply(
    records,
    function(record) {
      identical(record$status, "included")
    },
    logical(1)
  ))
  diagnostics <- list()
  if (length(eligible) == 0L) {
    return(list(records = records, diagnostics = diagnostics))
  }
  for (index in eligible) {
    records[[index]]$declared_compatibility <-
      glc_plan_declared_compatibility(records[[index]])
  }

  context_keys <- vapply(
    records[eligible],
    function(record) {
      glc_plan_canonical_text(glc_compatibility_context_contract(
        record$declared_compatibility
      ))
    },
    character(1)
  )
  for (key in unique(context_keys)) {
    indexes <- eligible[context_keys == key]
    diagnostics <- c(
      diagnostics,
      glc_selected_contract_diagnostics(
        records[indexes],
        request,
        provenance
      )
    )
  }

  base_keys <- vapply(
    records[eligible],
    function(record) {
      glc_plan_canonical_text(glc_compatibility_base_contract(
        record$declared_compatibility
      ))
    },
    character(1)
  )
  for (key in unique(base_keys)) {
    indexes <- eligible[base_keys == key]
    contracts <- lapply(records[indexes], function(record) {
      record$declared_compatibility
    })
    merged <- glc_compatibility_merge_contracts(contracts)
    for (result in merged$results) {
      diagnostic <- glc_factor_result_diagnostic(
        result,
        "selected",
        records[indexes],
        provenance,
        request$variable_scope
      )
      if (!is.null(diagnostic)) {
        diagnostics[[length(diagnostics) + 1L]] <- diagnostic
      }
    }
    partitions <- if (isTRUE(merged$compatible)) {
      list(list(indexes = indexes, contract = merged$contract))
    } else {
      exact_keys <- vapply(contracts, glc_plan_canonical_text, character(1))
      lapply(unique(exact_keys), function(exact_key) {
        member_indexes <- indexes[exact_keys == exact_key]
        list(
          indexes = member_indexes,
          contract = records[[member_indexes[[1L]]]]$declared_compatibility
        )
      })
    }
    for (partition in partitions) {
      compatibility_id <- glc_plan_compatibility_id(
        partition$contract,
        provenance
      )
      for (index in partition$indexes) {
        records[[index]]$family_compatibility_id <- compatibility_id
        records[[index]]$family_compatibility <- partition$contract
      }
    }
  }

  family_ids <- unique(vapply(
    records[eligible],
    function(record) {
      record$family_compatibility_id
    },
    character(1)
  ))
  for (family_id in family_ids) {
    indexes <- eligible[vapply(
      records[eligible],
      function(record) {
        identical(record$family_compatibility_id, family_id)
      },
      logical(1)
    )]
    selected_names <- vapply(
      records[[indexes[[1L]]]]$family_compatibility$variables,
      function(variable) variable$name,
      character(1)
    )
    diagnostics <- c(
      diagnostics,
      glc_nonselected_contract_diagnostics(
        records[indexes],
        selected_names,
        provenance
      )
    )
  }

  if (length(diagnostics) > 0L) {
    ids <- vapply(
      diagnostics,
      function(diagnostic) {
        diagnostic$diagnostic_id
      },
      character(1)
    )
    diagnostics <- diagnostics[!duplicated(ids)]
    diagnostics <- diagnostics[order(
      glc_plan_utf8_key(vapply(
        diagnostics,
        function(diagnostic) {
          diagnostic$diagnostic_id
        },
        character(1)
      )),
      method = "radix"
    )]
  }
  list(records = records, diagnostics = diagnostics)
}

glc_plan_empty_compatibility_diagnostics <- function() {
  tibble::tibble(
    diagnostic_id = character(),
    selection_scope = character(),
    classification = character(),
    code = character(),
    variable_name = character(),
    applies_to_current_plan = logical(),
    prospective_scopes = list(),
    message = character(),
    affected_group_count = integer(),
    affected_structure_count = integer(),
    affected_unit_count = integer(),
    file_group_ids = list(),
    compatibility_ids = list(),
    unit_ids = list(),
    union_values = list(),
    union_labels = list(),
    union_descriptions = list()
  )
}

glc_diagnostic_current_applies <- function(diagnostic, records) {
  if (!identical(diagnostic$selection_scope, "selected")) {
    return(FALSE)
  }
  affected <- records[vapply(
    records,
    function(record) {
      identical(record$status, "included") &&
        record$file_group_id %in% diagnostic$affected_file_group_ids
    },
    logical(1)
  )]
  if (length(affected) < 2L) {
    return(FALSE)
  }
  if (identical(diagnostic$classification, "blocking")) {
    ids <- unique(vapply(
      affected,
      function(record) {
        record$family_compatibility_id
      },
      character(1)
    ))
    return(length(ids) > 1L)
  }
  variable_name <- diagnostic$variable_name
  keys <- vapply(
    affected,
    function(record) {
      variables <- record$declared_compatibility$variables
      names <- vapply(variables, function(variable) variable$name, character(1))
      variable <- variables[[match(variable_name, names)]]
      glc_factor_contract_key(variable)
    },
    character(1)
  )
  length(unique(keys)) > 1L
}

glc_plan_compatibility_diagnostics_table <- function(diagnostics, records) {
  if (length(diagnostics) == 0L) {
    return(glc_plan_empty_compatibility_diagnostics())
  }
  dplyr::bind_rows(lapply(diagnostics, function(diagnostic) {
    affected <- records[vapply(
      records,
      function(record) {
        record$file_group_id %in% diagnostic$affected_file_group_ids
      },
      logical(1)
    )]
    compatibility_ids <- glc_plan_sort_utf8(unique(vapply(
      affected,
      function(record) record$family_compatibility_id %||% NA_character_,
      character(1)
    )))
    compatibility_ids <- compatibility_ids[
      !is.na(compatibility_ids) & nzchar(compatibility_ids)
    ]
    unit_ids <- glc_plan_sort_utf8(unique(vapply(
      affected,
      function(record) record$unit_id %||% NA_character_,
      character(1)
    )))
    unit_ids <- unit_ids[!is.na(unit_ids) & nzchar(unit_ids)]
    tibble::tibble(
      diagnostic_id = diagnostic$diagnostic_id,
      selection_scope = diagnostic$selection_scope,
      classification = diagnostic$classification,
      code = diagnostic$code,
      variable_name = diagnostic$variable_name,
      applies_to_current_plan = glc_diagnostic_current_applies(
        diagnostic,
        records
      ),
      prospective_scopes = list(diagnostic$prospective_scopes),
      message = diagnostic$message,
      affected_group_count = length(diagnostic$affected_file_group_ids),
      affected_structure_count = length(compatibility_ids),
      affected_unit_count = length(unit_ids),
      file_group_ids = list(diagnostic$affected_file_group_ids),
      compatibility_ids = list(compatibility_ids),
      unit_ids = list(unit_ids),
      union_values = list(diagnostic$union_values),
      union_labels = list(diagnostic$union_labels),
      union_descriptions = list(diagnostic$union_descriptions)
    )
  }))
}

glc_plan_empty_compatibility_diagnostic_groups <- function() {
  tibble::tibble(
    diagnostic_id = character(),
    dataset_id = character(),
    file_group_id = character(),
    current_status = character(),
    compatibility_id = character(),
    unit_id = character(),
    variable_present = logical(),
    selected_by_request = logical(),
    declaration_position = integer(),
    declared_type = character(),
    factor_values = list(),
    factor_labels = list(),
    factor_descriptions = list()
  )
}

glc_plan_compatibility_diagnostic_groups_table <- function(
  diagnostics,
  records
) {
  if (length(diagnostics) == 0L) {
    return(glc_plan_empty_compatibility_diagnostic_groups())
  }
  rows <- unlist(
    lapply(diagnostics, function(diagnostic) {
      affected <- records[vapply(
        records,
        function(record) {
          record$file_group_id %in% diagnostic$affected_file_group_ids
        },
        logical(1)
      )]
      ids <- vapply(
        affected,
        function(record) record$file_group_id,
        character(1)
      )
      affected <- affected[order(glc_plan_utf8_key(ids), method = "radix")]
      lapply(affected, function(record) {
        variable_names <- vapply(
          record$variables,
          function(variable) variable$name,
          character(1)
        )
        position <- match(diagnostic$variable_name, variable_names)
        present <- !is.na(position)
        variable <- if (present) record$variables[[position]] else NULL
        selected_names <- vapply(
          record$selected_variables,
          function(value) value$name,
          character(1)
        )
        list(
          diagnostic = diagnostic,
          record = record,
          variable = variable,
          present = present,
          selected = present && diagnostic$variable_name %in% selected_names,
          position = if (present) as.integer(position) else NA_integer_
        )
      })
    }),
    recursive = FALSE
  )
  if (length(rows) == 0L) {
    return(glc_plan_empty_compatibility_diagnostic_groups())
  }
  tibble::tibble(
    diagnostic_id = vapply(
      rows,
      function(row) {
        row$diagnostic$diagnostic_id
      },
      character(1)
    ),
    dataset_id = vapply(
      rows,
      function(row) {
        row$record$dataset_id
      },
      character(1)
    ),
    file_group_id = vapply(
      rows,
      function(row) {
        row$record$file_group_id
      },
      character(1)
    ),
    current_status = vapply(
      rows,
      function(row) {
        row$record$status
      },
      character(1)
    ),
    compatibility_id = vapply(
      rows,
      function(row) {
        row$record$family_compatibility_id %||% NA_character_
      },
      character(1)
    ),
    unit_id = vapply(
      rows,
      function(row) {
        row$record$unit_id %||% NA_character_
      },
      character(1)
    ),
    variable_present = vapply(rows, function(row) row$present, logical(1)),
    selected_by_request = vapply(rows, function(row) row$selected, logical(1)),
    declaration_position = vapply(
      rows,
      function(row) {
        row$position
      },
      integer(1)
    ),
    declared_type = vapply(
      rows,
      function(row) {
        if (row$present) row$variable$type else NA_character_
      },
      character(1)
    ),
    factor_values = lapply(rows, function(row) {
      if (row$present) {
        glc_factor_contract_variable(row$variable)$factor_values
      } else {
        character()
      }
    }),
    factor_labels = lapply(rows, function(row) {
      if (row$present) {
        glc_factor_contract_variable(row$variable)$factor_labels
      } else {
        character()
      }
    }),
    factor_descriptions = lapply(rows, function(row) {
      if (row$present) {
        glc_factor_contract_variable(row$variable)$factor_descriptions
      } else {
        character()
      }
    })
  )
}
