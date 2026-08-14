plan_validated_package <- function(
  root = make_glc_fixture("3.0.2"),
  commit = strrep("a", 40L),
  manifest_bytes = numeric()
) {
  package <- glc_open(root, quiet = TRUE)
  package$repo <- "owner/fixture"
  package$commit <- commit
  package$verified <- TRUE
  package$manifest <- list(
    manifest_version = "1.0",
    repository = package$repo,
    commit = commit,
    registry_verified = TRUE,
    files = lapply(seq_along(manifest_bytes), function(index) {
      list(
        path = names(manifest_bytes)[[index]],
        bytes = unname(manifest_bytes[[index]])
      )
    })
  )
  package
}

plan_remote_package <- function(
  package,
  latest_pass_commit = package$commit
) {
  glcdp:::glc_model(package)
  glcdp:::glc_plan_metadata_load_resources(package)
  package$source_type <- "remote"
  package$root <- NULL
  package$manifest <- NULL
  package$registry_row <- tibble::tibble(
    id = "fixture-package",
    latest_pass_commit = latest_pass_commit,
    registry_generated_at = "2026-07-20T12:00:00Z"
  )
  package
}

make_plan_matrix_fixture <- function(modifiers) {
  root <- make_glc_fixture("3.0.2")
  base <- fixture_read_datasets(root)[[1L]]
  datasets <- lapply(seq_along(modifiers), function(index) {
    dataset <- base
    dataset$dataset_internal_id <- paste0("DS", index)
    dataset$dataset_file[[1L]]$dataset_file_names <- list(paste0(
      "data/files/light-",
      index,
      ".csv"
    ))
    modifiers[[index]](dataset)
  })
  fixture_write_datasets(root, datasets)
  root
}

make_reserved_plan_fixture <- function() {
  root <- make_glc_fixture("3.0.2")
  datasets <- fixture_read_datasets(root)
  group <- datasets[[1L]]$dataset_file[[1L]]
  group$dataset_file_variables[[1L]]$dataset_file_variables_name <-
    ".glc_shadow"
  group$dataset_file_datetime$dataset_file_datetime_date <- ".glc_shadow"
  datasets[[1L]]$dataset_file[[1L]] <- group
  fixture_write_datasets(root, datasets)

  path <- file.path(root, "data", "files", "light.csv")
  lines <- readLines(path, warn = FALSE)
  lines[[1L]] <- sub("^timestamp", ".glc_shadow", lines[[1L]])
  writeLines(lines, path, useBytes = TRUE)
  root
}

make_plan_factor_fixture <- function(
  levels,
  labels = levels,
  descriptions = lapply(levels, function(values) {
    rep(NA_character_, length(values))
  }),
  reverse_datasets = FALSE
) {
  root <- make_glc_fixture("3.0.2")
  base <- fixture_read_datasets(root)[[1L]]
  datasets <- lapply(seq_along(levels), function(index) {
    dataset <- base
    dataset$dataset_internal_id <- paste0("DS", index)
    variables <- dataset$dataset_file[[1L]]$dataset_file_variables
    quality <- which(vapply(
      variables,
      function(variable) {
        identical(variable$dataset_file_variables_name, "quality")
      },
      logical(1)
    ))[[1L]]
    variables[[quality]]$dataset_file_variables_factor_levels <- lapply(
      seq_along(levels[[index]]),
      function(position) {
        list(
          value = levels[[index]][[position]],
          label = labels[[index]][[position]],
          description = descriptions[[index]][[position]]
        )
      }
    )
    dataset$dataset_file[[1L]]$dataset_file_variables <- variables
    dataset
  })
  if (reverse_datasets) {
    datasets <- rev(datasets)
  }
  fixture_write_datasets(root, datasets)
  root
}

plan_has_live_value <- function(x) {
  if (
    is.environment(x) ||
      is.function(x) ||
      is.language(x) ||
      typeof(x) %in% c("externalptr", "weakref")
  ) {
    return(TRUE)
  }
  if (is.list(x)) {
    return(any(vapply(x, plan_has_live_value, logical(1))))
  }
  FALSE
}

expect_explorer_plan_parity <- function(
  package,
  plan,
  variable_names = character(),
  variable_terms = character(),
  standardize = "lightlogr",
  dataset_id = character(),
  file_group = character()
) {
  groups <- glcdp:::glc_explorer_group_inventory(package)
  engine <- glcdp:::glc_explorer_declared_engine(
    groups,
    variable_names,
    variable_terms,
    package = package,
    dataset_id = dataset_id,
    file_group = file_group,
    standardize = standardize
  )
  engine_ids <- vapply(
    engine$records,
    function(record) record$file_group_id,
    character(1)
  )
  engine_status <- vapply(
    engine$records,
    function(record) record$status,
    character(1)
  )
  engine_unit_ids <- vapply(
    engine$records,
    function(record) record$unit_id,
    character(1)
  )
  engine_reasons <- lapply(engine$records, function(record) {
    vapply(record$reasons, function(reason) reason$code, character(1))
  })
  engine_messages <- lapply(engine$records, function(record) {
    vapply(record$reasons, function(reason) reason$message, character(1))
  })

  expect_identical(engine_ids, plan$groups$file_group_id)
  expect_identical(engine_status, plan$groups$status)
  expect_identical(engine_unit_ids, plan$groups$unit_id)
  expect_identical(engine_reasons, plan$groups$reason_codes)
  expect_identical(engine_messages, plan$groups$messages)
  expect_identical(
    vapply(engine$units, function(unit) unit$unit_id, character(1)),
    plan$units$unit_id
  )
  expect_identical(
    lapply(engine$units, function(unit) unit$file_group_ids),
    plan$units$file_group_ids
  )
  expect_identical(engine$preferred_unit_id, plan$preferred_unit_id)

  reordered <- groups[rev(seq_len(nrow(groups))), , drop = FALSE]
  reordered_engine <- glcdp:::glc_explorer_declared_engine(
    reordered,
    variable_names,
    variable_terms,
    package = package,
    dataset_id = dataset_id,
    file_group = file_group,
    standardize = standardize
  )
  expect_identical(reordered_engine$records, engine$records)
  expect_identical(reordered_engine$units, engine$units)
  expect_identical(
    reordered_engine$preferred_unit_id,
    engine$preferred_unit_id
  )
  invisible(engine)
}

test_that("glc_collection_plan has the approved public interface", {
  expect_true("glc_collection_plan" %in% getNamespaceExports("glcdp"))
  expect_identical(
    getExportedValue("glcdp", "glc_collection_plan"),
    glc_collection_plan
  )
  expect_identical(
    names(formals(glc_collection_plan)),
    c(
      "x",
      "terms",
      "variable_scope",
      "variables",
      "dataset_id",
      "file_group",
      "standardize"
    )
  )
  expect_null(formals(glc_collection_plan)$terms)
  expect_identical(
    eval(formals(glc_collection_plan)$variable_scope, baseenv()),
    c("matched", "all", "selected")
  )
  expect_null(formals(glc_collection_plan)$variables)
  expect_null(formals(glc_collection_plan)$dataset_id)
  expect_null(formals(glc_collection_plan)$file_group)
  expect_identical(
    eval(formals(glc_collection_plan)$standardize, baseenv()),
    c("lightlogr", "none")
  )
})

test_that("a metadata-only plan has the approved structure and print contract", {
  root <- make_glc_fixture("3.0.2")
  package <- plan_validated_package(
    root,
    manifest_bytes = c("data/files/light.csv" = 321)
  )
  package$transport$token <- "secret-token"
  package$transport$cache_dir <- "/private/secret-cache"

  plan <- glc_collection_plan(
    package,
    terms = "photopic illuminance"
  )

  expect_s3_class(plan, "glc_collection_plan")
  expect_identical(
    names(plan),
    c(
      "plan_schema",
      "plan_version",
      "provenance",
      "request",
      "assurance",
      "preferred_unit_id",
      "units",
      "groups",
      "variables",
      "read_columns",
      "output_columns",
      "files",
      "compatibility",
      "extensions",
      "compatibility_sets",
      "compatibility_diagnostics",
      "compatibility_diagnostic_groups",
      "metadata",
      "refinement_input"
    )
  )
  expect_identical(
    names(plan$units),
    c(
      "unit_id",
      "compatibility_id",
      "preferred",
      "dataset_count",
      "file_group_count",
      "variable_count",
      "file_count",
      "declared_bytes",
      "known_file_count",
      "unknown_file_count",
      "declared_bytes_complete",
      "harmonization_required",
      "harmonized_variables",
      "diagnostic_ids",
      "file_group_ids"
    )
  )
  expect_identical(
    names(plan$groups),
    c(
      "status",
      "unit_id",
      "compatibility_id",
      "dataset_id",
      "file_group",
      "file_group_id",
      "study_id",
      "participant_id",
      "participant_associated",
      "study_link_status",
      "participant_link_status",
      "device_id",
      "device_link_status",
      "datasheet_id",
      "device_location",
      "device_location_type",
      "description",
      "instructions",
      "instrument_declared",
      "dataset_timezone",
      "dataset_latitude",
      "dataset_longitude",
      "format",
      "timezone",
      "modalities",
      "modality_other",
      "modality_other_type",
      "role",
      "data_state",
      "temporal_type",
      "temporal_value",
      "temporal_unit",
      "header_row",
      "preprocessing",
      "datetime_source",
      "datetime_date",
      "datetime_format",
      "datetime_time",
      "datetime_time_format",
      "selected_variables",
      "reason_codes",
      "messages"
    )
  )
  expect_identical(
    names(plan$variables),
    c(
      "unit_id",
      "dataset_id",
      "file_group_id",
      "position",
      "name",
      "label",
      "description",
      "unit",
      "calibration",
      "type",
      "term",
      "term_name",
      "primary",
      "factor_values",
      "factor_labels",
      "factor_descriptions",
      "selection_origin"
    )
  )
  expect_identical(
    names(plan$read_columns),
    c(
      "unit_id",
      "dataset_id",
      "file_group_id",
      "position",
      "name",
      "declared_type",
      "origin",
      "selected_for_output",
      "automatic",
      "message"
    )
  )
  expect_identical(
    names(plan$output_columns),
    c(
      "unit_id",
      "position",
      "name",
      "source_declared_type",
      "expected_type",
      "origin",
      "automatic",
      "runtime_validation_required",
      "collision_validation_required",
      "message"
    )
  )
  expect_identical(
    names(plan$files),
    c(
      "status",
      "unit_id",
      "dataset_id",
      "file_group_id",
      "position",
      "declared_path",
      "format",
      "encoding",
      "declared_bytes",
      "bytes_known"
    )
  )
  expect_identical(
    names(plan$compatibility),
    c(
      "unit_id",
      "selected_names",
      "declared_types",
      "factor_values",
      "factor_labels",
      "factor_descriptions",
      "harmonization_required",
      "harmonized_variables",
      "diagnostic_ids",
      "timezone",
      "modalities",
      "role",
      "data_state",
      "datetime_source",
      "datetime_signature",
      "datetime_date",
      "datetime_format",
      "datetime_time",
      "datetime_time_format",
      "collection_values_ignored",
      "relationship_rule",
      "device_rule",
      "standardize",
      "standardize_affects_partition"
    )
  )
  expect_identical(
    names(plan$compatibility_sets),
    c(
      "compatibility_id",
      "dataset_count",
      "file_group_count",
      "variable_count",
      "file_count",
      "declared_bytes",
      "known_file_count",
      "unknown_file_count",
      "declared_bytes_complete",
      "harmonization_required",
      "harmonized_variables",
      "diagnostic_ids",
      "final_unit_count",
      "final_selection_required",
      "constraint_codes",
      "constraint_messages",
      "file_group_ids",
      "final_unit_ids",
      "selected_names",
      "declared_types",
      "factor_values",
      "factor_labels",
      "factor_descriptions",
      "timezone",
      "modalities",
      "role",
      "data_state",
      "datetime_source",
      "datetime_signature",
      "datetime_date",
      "datetime_format",
      "datetime_time",
      "datetime_time_format",
      "collection_values_ignored",
      "relationship_rule",
      "device_rule",
      "standardize_affects_structure"
    )
  )
  expect_identical(
    names(plan$extensions),
    c("dataset_id", "file_group_id", "metadata")
  )
  expect_identical(
    names(plan$compatibility_diagnostics),
    c(
      "diagnostic_id",
      "selection_scope",
      "classification",
      "code",
      "variable_name",
      "applies_to_current_plan",
      "prospective_scopes",
      "message",
      "affected_group_count",
      "affected_structure_count",
      "affected_unit_count",
      "file_group_ids",
      "compatibility_ids",
      "unit_ids",
      "union_values",
      "union_labels",
      "union_descriptions"
    )
  )
  expect_identical(
    names(plan$compatibility_diagnostic_groups),
    c(
      "diagnostic_id",
      "dataset_id",
      "file_group_id",
      "current_status",
      "compatibility_id",
      "unit_id",
      "variable_present",
      "selected_by_request",
      "declaration_position",
      "declared_type",
      "factor_values",
      "factor_labels",
      "factor_descriptions"
    )
  )
  expect_identical(plan$plan_schema, "glc-collection-plan")
  expect_identical(plan$plan_version, "1.2.0")
  expect_identical(plan$provenance$source_revision, strrep("a", 40L))
  expect_identical(plan$provenance$package_schema_version, "3.0.2")
  expect_identical(
    plan$provenance$verification,
    "manifest_registry_verification"
  )
  expect_match(plan$provenance$metadata_fingerprint, "^[0-9a-f]{64}$")
  expect_identical(plan$request$requested_variables, character())
  expect_identical(plan$request$resolved_variables, "lux")
  expect_false(plan$request$labels_used_for_matching)
  expect_identical(plan$assurance$basis, "validated_declarations")
  expect_identical(plan$assurance$actual_data_status, "not_checked")
  expect_false(plan$assurance$measurement_contents_transferred)
  expect_false(plan$assurance$measurement_contents_inspected)
  expect_identical(
    plan$assurance$final_validation,
    c(
      "glc_read",
      "glc_collect"
    )
  )

  expect_equal(nrow(plan$units), 1L)
  expect_match(plan$units$unit_id, "^glcu_[0-9a-f]{64}$")
  expect_identical(
    plan$units$unit_id,
    "glcu_e37925c31c7587c0732c3359dd21c07780e635c7bbbe9bb68fe9b641a0f3e878"
  )
  expect_true(plan$units$preferred)
  expect_match(plan$units$compatibility_id, "^glcc_[0-9a-f]{64}$")
  expect_identical(
    plan$units$compatibility_id,
    plan$compatibility_sets$compatibility_id
  )
  expect_identical(
    plan$groups$compatibility_id,
    plan$compatibility_sets$compatibility_id
  )
  expect_identical(
    plan$compatibility_sets$final_unit_ids[[1L]],
    plan$units$unit_id
  )
  expect_false(plan$compatibility_sets$final_selection_required)
  expect_equal(plan$units$declared_bytes, 321)
  expect_equal(plan$units$unknown_file_count, 0L)
  expect_identical(plan$groups$reason_codes[[1L]], "included")
  expect_identical(plan$variables$name, "lux")
  expect_identical(plan$read_columns$name, c("lux", "timestamp"))
  expect_identical(plan$read_columns$automatic, c(FALSE, TRUE))
  expect_identical(
    plan$output_columns$name,
    c(
      "lux",
      "Id",
      "file_group_id",
      "participant_Id",
      "Datetime",
      "file.name"
    )
  )
  expect_equal(plan$files$declared_bytes, 321)
  expect_identical(plan$metadata$schema, "glc-package-metadata")
  expect_identical(plan$metadata$version, "1.0.0")
  expect_identical(
    plan$metadata$resource_status$resource,
    glcdp:::glc_core_resource_names()
  )
  expect_identical(
    plan$refinement_input$schema,
    "glc-collection-refinement-input"
  )
  expect_identical(
    names(plan$refinement_input),
    c(
      "schema",
      "version",
      "canonicalization",
      "digest_algorithm",
      "provenance",
      "request",
      "membership",
      "contracts",
      "groups",
      "fingerprint"
    )
  )
  expect_identical(
    names(plan$refinement_input$membership),
    c("file_group_id", "status", "compatibility_id")
  )
  expect_match(plan$refinement_input$fingerprint, "^[0-9a-f]{64}$")

  printed <- capture_output(print(plan))
  expect_match(printed, "<GLC collection plan>", fixed = TRUE)
  expect_match(printed, "validated declarations only", fixed = TRUE)
  expect_match(
    printed,
    "1 final unit(s) in 1 structural set(s); 1 included, 0 excluded",
    fixed = TRUE
  )
  expect_false(grepl(root, printed, fixed = TRUE))
  expect_false(plan_has_live_value(plan))
  roundtrip <- unserialize(serialize(plan, NULL, version = 2L))
  expect_identical(roundtrip$preferred_unit_id, plan$preferred_unit_id)
  json <- jsonlite::toJSON(
    plan,
    dataframe = "rows",
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )
  expect_false(grepl("secret-token", json, fixed = TRUE))
  expect_false(grepl("secret-cache", json, fixed = TRUE))
  expect_false(grepl(root, json, fixed = TRUE))
})

test_that("unit ids and table order ignore declaration row reordering", {
  root <- make_plan_matrix_fixture(list(identity, identity, identity))
  first_package <- plan_validated_package(root)
  second_package <- plan_validated_package(root)
  first_model <- glcdp:::glc_model(first_package)
  second_model <- glcdp:::glc_model(second_package)
  second_model$datasets <- rev(second_model$datasets)
  second_package$transport$model <- second_model

  first <- glc_collection_plan(
    first_package,
    terms = c("other", "photopic illuminance")
  )
  second <- glc_collection_plan(
    second_package,
    terms = c("photopic illuminance", "other")
  )

  expect_identical(first$units, second$units)
  expect_identical(first$groups, second$groups)
  expect_identical(first$variables, second$variables)
  expect_identical(
    first$provenance$metadata_fingerprint,
    second$provenance$metadata_fingerprint
  )
})

test_that("dataset-only restrictions preserve planner identity without groups", {
  root <- make_plan_matrix_fixture(list(identity, identity, identity))
  package <- plan_validated_package(root)
  dataset_id <- c("DS3", "DS1")
  plan <- glc_collection_plan(
    package,
    terms = "photopic illuminance",
    dataset_id = dataset_id
  )
  engine <- expect_explorer_plan_parity(
    package,
    plan,
    variable_terms = "photopic illuminance",
    dataset_id = dataset_id
  )
  groups <- glcdp:::glc_explorer_group_inventory(package)
  candidate_groups <- groups[groups$dataset_id %in% dataset_id, , drop = FALSE]
  restrictions <- glcdp:::glc_explorer_planner_restrictions(
    dataset_id,
    candidate_groups
  )

  expect_identical(restrictions$dataset_id, c("DS1", "DS3"))
  expect_identical(restrictions$file_group, character())
  expect_identical(restrictions$file_group_basis, "omitted")
  expect_true(
    "scope_dataset" %in%
      vapply(
        engine$records[[2L]]$reasons,
        function(reason) reason$code,
        character(1)
      )
  )
})

test_that("explicit-all groups retain distinct canonical request identity", {
  root <- make_plan_matrix_fixture(list(
    identity,
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_timezone <- "UTC"
      dataset
    }
  ))
  package <- plan_validated_package(root)
  groups <- glcdp:::glc_explorer_group_inventory(package)
  file_group <- groups$file_group_id
  omitted <- glc_collection_plan(
    package,
    terms = "photopic illuminance"
  )
  explicit <- glc_collection_plan(
    package,
    terms = "photopic illuminance",
    file_group = file_group
  )
  omitted_engine <- expect_explorer_plan_parity(
    package,
    omitted,
    variable_terms = "photopic illuminance"
  )
  explicit_engine <- expect_explorer_plan_parity(
    package,
    explicit,
    variable_terms = "photopic illuminance",
    file_group = file_group
  )

  expect_equal(nrow(explicit$units), 2L)
  expect_false(identical(omitted$units$unit_id, explicit$units$unit_id))
  expect_identical(
    omitted$compatibility_sets$compatibility_id,
    explicit$compatibility_sets$compatibility_id
  )
  expect_false(identical(
    omitted_engine$preferred_unit_id,
    explicit_engine$preferred_unit_id
  ))

  filtered <- glcdp:::glc_explorer_filter_compatible_groups(
    groups,
    variable_terms = "photopic illuminance",
    package = package,
    declaration_groups = groups,
    file_group = file_group
  )
  reordered <- glcdp:::glc_explorer_filter_compatible_groups(
    groups[rev(seq_len(nrow(groups))), , drop = FALSE],
    variable_terms = "photopic illuminance",
    package = package,
    declaration_groups = groups,
    file_group = rev(file_group)
  )
  preferred_groups <- explicit$units$file_group_ids[[which(
    explicit$units$preferred
  )]]
  expect_setequal(filtered$groups$file_group_id, preferred_groups)
  expect_setequal(reordered$groups$file_group_id, preferred_groups)
  expect_identical(
    filtered$planner_request$file_group,
    glcdp:::glc_plan_sort_utf8(file_group)
  )
})

test_that("structural ids describe contracts rather than restricted membership", {
  root <- make_plan_matrix_fixture(list(identity, identity, identity))
  package <- plan_validated_package(root)
  parent <- glc_collection_plan(
    package,
    terms = "photopic illuminance"
  )
  selected_ids <- c("DS3:1", "DS1:1")
  narrowed <- glc_collection_plan(
    package,
    terms = "photopic illuminance",
    file_group = selected_ids
  )
  reordered <- glc_collection_plan(
    package,
    terms = "photopic illuminance",
    file_group = rev(selected_ids)
  )

  expect_equal(nrow(parent$compatibility_sets), 1L)
  expect_identical(
    narrowed$compatibility_sets$compatibility_id,
    parent$compatibility_sets$compatibility_id
  )
  expect_identical(
    reordered$compatibility_sets,
    narrowed$compatibility_sets
  )
  expect_identical(
    narrowed$compatibility_sets$file_group_ids[[1L]],
    c("DS1:1", "DS3:1")
  )
  expect_false(identical(parent$units$unit_id, narrowed$units$unit_id))
  expect_true(all(
    narrowed$groups$compatibility_id[narrowed$groups$status == "included"] ==
      parent$compatibility_sets$compatibility_id
  ))
})

test_that("narrower Explorer filters translate to stable group restrictions", {
  root <- make_plan_matrix_fixture(list(identity, identity, identity))
  package <- plan_validated_package(root)
  groups <- glcdp:::glc_explorer_group_inventory(package)
  dataset_id <- groups$dataset_id
  candidate_groups <- groups[groups$dataset_id != "DS2", , drop = FALSE]
  participant <- glcdp:::glc_explorer_planner_restrictions(
    dataset_id,
    candidate_groups,
    participant_restricted = TRUE
  )
  device <- glcdp:::glc_explorer_planner_restrictions(
    dataset_id,
    candidate_groups,
    device_restricted = TRUE
  )
  group_field <- glcdp:::glc_explorer_planner_restrictions(
    dataset_id,
    candidate_groups,
    group_filter_active = TRUE
  )

  expect_identical(participant, device)
  expect_identical(device, group_field)
  expect_identical(
    participant$file_group,
    c("DS1:1", "DS3:1")
  )
  expect_identical(
    participant$file_group_basis,
    "translated_candidate_universe"
  )

  plan <- glc_collection_plan(
    package,
    terms = "photopic illuminance",
    dataset_id = participant$dataset_id,
    file_group = participant$file_group
  )
  expect_explorer_plan_parity(
    package,
    plan,
    variable_terms = "photopic illuminance",
    dataset_id = participant$dataset_id,
    file_group = participant$file_group
  )

  reordered <- glcdp:::glc_explorer_planner_restrictions(
    rev(dataset_id),
    candidate_groups[rev(seq_len(nrow(candidate_groups))), , drop = FALSE],
    participant_restricted = TRUE
  )
  expect_identical(reordered, participant)
})

test_that("term discovery and all variable scopes are explicit", {
  root <- make_glc_fixture("3.0.2")
  datasets <- fixture_read_datasets(root)
  variables <- datasets[[1L]]$dataset_file[[1L]]$dataset_file_variables
  variables[[2L]]$dataset_file_variables_labels <- "Duplicate label"
  variables[[3L]]$dataset_file_variables_labels <- "Duplicate label"
  second_lux <- variables[[2L]]
  second_lux$dataset_file_variables_name <- "lux_secondary"
  variables <- append(variables, list(second_lux), after = 2L)
  datasets[[1L]]$dataset_file[[1L]]$dataset_file_variables <- variables
  fixture_write_datasets(root, datasets)
  package <- plan_validated_package(root)

  matched <- glc_collection_plan(
    package,
    terms = "photopic illuminance",
    variable_scope = "matched"
  )
  expect_identical(
    matched$variables$name,
    c("lux", "lux_secondary")
  )
  expect_identical(
    matched$variables$label,
    c("Duplicate label", "Duplicate label")
  )
  expect_explorer_plan_parity(
    package,
    matched,
    variable_terms = "photopic illuminance"
  )

  all <- glc_collection_plan(package, variable_scope = "all")
  expect_identical(
    all$variables$name,
    c("timestamp", "lux", "lux_secondary", "worn", "quality")
  )
  expect_explorer_plan_parity(package, all)

  selected <- glc_collection_plan(
    package,
    variable_scope = "selected",
    variables = c("quality", "lux")
  )
  expect_identical(selected$request$requested_variables, c("lux", "quality"))
  expect_identical(selected$variables$name, c("lux", "quality"))
  expect_explorer_plan_parity(
    package,
    selected,
    variable_names = c("quality", "lux")
  )

  independent <- glc_collection_plan(
    package,
    terms = "photopic illuminance",
    variable_scope = "selected",
    variables = "worn"
  )
  expect_identical(independent$variables$name, "worn")
  expect_explorer_plan_parity(
    package,
    independent,
    variable_names = "worn",
    variable_terms = "photopic illuminance"
  )

  expect_error(
    glc_collection_plan(package, terms = "Duplicate label"),
    "Labels are display metadata",
    class = "glcdp_unknown_term"
  )
  expect_error(
    glc_collection_plan(package, terms = "unknown-term"),
    "Unknown canonical",
    class = "glcdp_unknown_term"
  )
})

test_that("all-of term and selected-variable misses remain explained", {
  root <- make_plan_matrix_fixture(list(
    identity,
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_variables[[2L]][[
        "dataset_file_variables_term"
      ]]$variable_term <- "other"
      dataset
    },
    function(dataset) {
      variables <- dataset$dataset_file[[1L]]$dataset_file_variables
      dataset$dataset_file[[1L]]$dataset_file_variables <- variables[
        vapply(
          variables,
          function(variable) {
            variable$dataset_file_variables_name != "lux"
          },
          logical(1)
        )
      ]
      dataset
    }
  ))
  package <- plan_validated_package(root)

  terms <- glc_collection_plan(
    package,
    terms = c("photopic illuminance", "other")
  )
  expect_identical(
    terms$groups$status,
    c(
      "included",
      "excluded",
      "excluded"
    )
  )
  expect_true("term_missing" %in% terms$groups$reason_codes[[2L]])
  expect_true("term_missing" %in% terms$groups$reason_codes[[3L]])
  expect_explorer_plan_parity(
    package,
    terms,
    variable_terms = c("photopic illuminance", "other")
  )

  selected <- glc_collection_plan(
    package,
    variable_scope = "selected",
    variables = "lux"
  )
  expect_identical(
    selected$groups$status,
    c(
      "included",
      "included",
      "excluded"
    )
  )
  expect_true("variable_missing" %in% selected$groups$reason_codes[[3L]])
  expect_explorer_plan_parity(
    package,
    selected,
    variable_names = "lux"
  )

  empty <- glc_collection_plan(
    package,
    terms = "photopic illuminance",
    dataset_id = "DS2"
  )
  expect_equal(nrow(empty$units), 0L)
  expect_true(all(empty$groups$status == "excluded"))
  expect_true(is.na(empty$preferred_unit_id))
})

test_that("declared compatibility dimensions partition units", {
  root <- make_plan_matrix_fixture(list(
    identity,
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_timezone <- "UTC"
      dataset
    },
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_modality <- list(
        "light",
        "accelerometry"
      )
      dataset
    },
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_modality <- list(
        "accelerometry",
        "light"
      )
      dataset
    },
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_role <- "supporting"
      dataset
    },
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_data_state <- "processed"
      dataset
    },
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_datetime[[
        "dataset_file_datetime_dateformat"
      ]] <- "DD/MM/YYYY HH:mm:ss"
      dataset
    },
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_variables[[2L]][[
        "dataset_file_variables_type"
      ]] <- "integer"
      dataset
    },
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_variables[[4L]][[
        "dataset_file_variables_factor_levels"
      ]][[1L]]$label <- "Accepted"
      dataset
    }
  ))
  package <- plan_validated_package(root)
  plan <- glc_collection_plan(package, variable_scope = "all")

  expect_equal(nrow(plan$units), 9L)
  expect_equal(nrow(plan$compatibility_sets), 9L)
  expect_equal(length(unique(plan$groups$unit_id)), 9L)
  expect_setequal(
    plan$compatibility$timezone,
    c("Europe/Berlin", "UTC")
  )
  expect_true(any(plan$compatibility$role == "supporting"))
  expect_true(any(plan$compatibility$data_state == "processed"))
  expect_true(any(vapply(
    plan$compatibility$declared_types,
    function(types) {
      "integer" %in% types
    },
    logical(1)
  )))
  expect_setequal(
    plan$compatibility_sets$timezone,
    c("Europe/Berlin", "UTC")
  )
  expect_true(any(plan$compatibility_sets$role == "supporting"))
  expect_true(any(plan$compatibility_sets$data_state == "processed"))
  expect_true(any(vapply(
    plan$compatibility_sets$declared_types,
    function(types) "integer" %in% types,
    logical(1)
  )))
  engine <- expect_explorer_plan_parity(package, plan)
  expect_setequal(
    glcdp:::glc_declared_collection_record_differences(engine$records),
    c(
      "types_or_factor_levels",
      "timezone",
      "modalities",
      "role",
      "data_state",
      "datetime"
    )
  )
})

test_that("descriptive facets do not partition structural compatibility", {
  root <- make_plan_matrix_fixture(list(
    identity,
    function(dataset) {
      group <- dataset$dataset_file[[1L]]
      group$dataset_file_description <- "Different description"
      group$dataset_file_device_location <- "chest"
      group$dataset_file_device_location_type <- "body_worn"
      group$dataset_file_instructions <- "Different instructions"
      group$dataset_file_instrument <- list(
        instrument_type = "sensor",
        instrument_name = "Different instrument"
      )
      dataset$dataset_location <- list(1, 2)
      dataset$dataset_file[[1L]] <- group
      dataset
    }
  ))
  plan <- glc_collection_plan(
    plan_validated_package(root),
    variable_scope = "all"
  )

  expect_equal(nrow(plan$compatibility_sets), 1L)
  expect_equal(nrow(plan$units), 1L)
  expect_identical(
    unique(plan$groups$compatibility_id),
    plan$compatibility_sets$compatibility_id
  )
  expect_setequal(plan$groups$description, c(NA, "Different description"))
  expect_setequal(plan$groups$device_location, c("non-dominant wrist", "chest"))
})

test_that("safe factor unions form one stable structural family", {
  levels <- list(
    c("0", "1", "2", "3", "5", "10", "11", "12"),
    c("0", "1", "2", "3", "4"),
    c("0", "1", "2", "3")
  )
  root <- make_plan_factor_fixture(levels)
  reordered_root <- make_plan_factor_fixture(levels, reverse_datasets = TRUE)
  package <- plan_validated_package(root)
  reordered_package <- plan_validated_package(reordered_root)

  plan <- glc_collection_plan(package, variable_scope = "all")
  reordered <- glc_collection_plan(
    reordered_package,
    variable_scope = "all"
  )

  expect_equal(nrow(plan$compatibility_sets), 1L)
  expect_equal(nrow(plan$units), 1L)
  expect_true(plan$units$harmonization_required)
  expect_identical(plan$units$harmonized_variables[[1L]], "quality")
  expect_identical(
    plan$compatibility_sets$factor_values[[1L]][[4L]],
    c("0", "1", "2", "3", "4", "5", "10", "11", "12")
  )
  selected <- plan$compatibility_diagnostics$selection_scope == "selected"
  expect_identical(
    plan$compatibility_diagnostics$code[selected],
    "factor_level_union"
  )
  expect_identical(
    plan$compatibility_diagnostics$classification[selected],
    "safely_harmonizable"
  )
  expect_true(plan$compatibility_diagnostics$applies_to_current_plan[selected])
  expect_identical(
    plan$compatibility_sets$compatibility_id,
    reordered$compatibility_sets$compatibility_id
  )
  expect_identical(plan$units$unit_id, reordered$units$unit_id)
  expect_identical(
    plan$compatibility_diagnostics$diagnostic_id,
    reordered$compatibility_diagnostics$diagnostic_id
  )

  restricted <- glc_collection_plan(
    package,
    variable_scope = "all",
    file_group = "DS1:1"
  )
  expect_identical(
    restricted$compatibility_sets$compatibility_id,
    plan$compatibility_sets$compatibility_id
  )
  expect_identical(
    restricted$compatibility_sets$factor_values[[1L]][[4L]],
    plan$compatibility_sets$factor_values[[1L]][[4L]]
  )
  expect_identical(
    restricted$compatibility$factor_values[[1L]][[4L]],
    levels[[1L]]
  )
  expect_false(restricted$units$harmonization_required)
})

test_that("non-selected factor differences are disclosed without selection", {
  root <- make_plan_factor_fixture(list(
    c("good", "bad"),
    c("good", "bad", "maybe")
  ))
  plan <- glc_collection_plan(
    plan_validated_package(root),
    terms = "photopic illuminance",
    variable_scope = "matched"
  )

  expect_equal(nrow(plan$compatibility_sets), 1L)
  expect_equal(nrow(plan$units), 1L)
  expect_identical(plan$variables$name, rep("lux", 2L))
  diagnostic <- plan$compatibility_diagnostics[
    plan$compatibility_diagnostics$variable_name == "quality",
  ]
  expect_equal(nrow(diagnostic), 1L)
  expect_identical(diagnostic$selection_scope, "not_selected")
  expect_identical(diagnostic$classification, "safely_harmonizable")
  expect_false(diagnostic$applies_to_current_plan)
  expect_match(diagnostic$message, "not selected", fixed = TRUE)
  expect_true(all(
    !plan$compatibility_diagnostic_groups$selected_by_request
  ))
})

test_that("conflicting factor mappings remain separate and diagnostic", {
  plan <- glc_collection_plan(
    plan_validated_package(make_plan_factor_fixture(
      levels = list(c("0", "1"), c("0", "1")),
      labels = list(c("Off", "On"), c("Absent", "On"))
    )),
    variable_scope = "all"
  )

  expect_equal(nrow(plan$compatibility_sets), 2L)
  expect_equal(nrow(plan$units), 2L)
  diagnostic <- plan$compatibility_diagnostics[
    plan$compatibility_diagnostics$code == "factor_label_conflict",
  ]
  expect_equal(nrow(diagnostic), 1L)
  expect_identical(diagnostic$classification, "blocking")
  expect_true(diagnostic$applies_to_current_plan)
  expect_equal(diagnostic$affected_group_count, 2L)
  expect_equal(diagnostic$affected_structure_count, 2L)
  expect_setequal(
    diagnostic$file_group_ids[[1L]],
    c("DS1:1", "DS2:1")
  )
})

test_that("invalid factor declarations are excluded before unit construction", {
  plan <- glc_collection_plan(
    plan_validated_package(make_plan_factor_fixture(list(
      c("good", "good"),
      c("good", "bad")
    ))),
    variable_scope = "all"
  )

  invalid <- plan$groups$file_group_id == "DS1:1"
  expect_identical(plan$groups$status[invalid], "excluded")
  expect_true(
    "invalid_factor_contract" %in% plan$groups$reason_codes[[which(invalid)]]
  )
  expect_true(is.na(plan$groups$unit_id[invalid]))
  expect_equal(nrow(plan$units), 1L)
})

test_that("collection datetime values are masked in compatibility", {
  root <- make_plan_matrix_fixture(list(
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_datetime <- list(
        dataset_file_datetime_source = "collection",
        dataset_file_datetime_date = "2026-01-01 08:00:00",
        dataset_file_datetime_dateformat = "YYYY-MM-DD HH:mm:ss"
      )
      dataset
    },
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_datetime <- list(
        dataset_file_datetime_source = "collection",
        dataset_file_datetime_date = "2030-12-31 23:59:00",
        dataset_file_datetime_dateformat = "YYYY-MM-DD HH:mm:ss"
      )
      dataset
    }
  ))
  package <- plan_validated_package(root)
  plan <- glc_collection_plan(package, variable_scope = "all")

  expect_equal(nrow(plan$units), 1L)
  expect_true(plan$compatibility$collection_values_ignored)
  expect_identical(
    plan$compatibility$datetime_date,
    "<collection-value>"
  )
})

test_that("device identity is scoped by stable file group", {
  root <- make_glc_fixture("3.0.2")
  datasets <- fixture_read_datasets(root)
  group <- datasets[[1L]]$dataset_file[[1L]]
  group_one <- group
  group_one$dataset_file_crossref_device_id <- "D2"
  group_one$dataset_file_names <- list("data/files/device-2.csv")
  group_two <- group
  group_two$dataset_file_crossref_device_id <- "D1"
  group_two$dataset_file_names <- list("data/files/device-1.csv")
  group_three <- group
  group_three$dataset_file_crossref_device_id <- NULL
  group_three$dataset_file_names <- list("data/files/device-missing.csv")
  datasets[[1L]]$dataset_file <- list(group_one, group_two, group_three)
  fixture_write_datasets(root, datasets)

  plan <- glc_collection_plan(
    plan_validated_package(root),
    variable_scope = "all"
  )

  expect_equal(nrow(plan$units), 1L)
  expect_equal(nrow(plan$compatibility_sets), 1L)
  expect_equal(plan$compatibility_sets$final_unit_count, 1L)
  expect_false(plan$compatibility_sets$final_selection_required)
  expect_identical(plan$compatibility_sets$constraint_codes[[1L]], character())
  expect_setequal(
    plan$compatibility_sets$final_unit_ids[[1L]],
    plan$units$unit_id
  )
  expect_true(all(
    plan$groups$compatibility_id == plan$compatibility_sets$compatibility_id
  ))
  expect_length(unique(plan$groups$unit_id), 1L)
  expect_identical(
    plan$compatibility$device_rule,
    "device_identity_is_file_group_scoped"
  )
  engine <- expect_explorer_plan_parity(
    plan_validated_package(root),
    plan
  )
  expect_identical(
    glcdp:::glc_declared_collection_record_differences(engine$records),
    character()
  )
})

test_that("unsupported and incomplete declarations are excluded with reasons", {
  root <- make_plan_matrix_fixture(list(
    identity,
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_format <- "rds"
      dataset
    },
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_timezone <- "Mars/Olympus"
      dataset
    },
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_datetime[[
        "dataset_file_datetime_dateformat"
      ]] <- NULL
      dataset
    },
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_names <- list()
      dataset$dataset_file[[1L]]$dataset_file_encoding <- list("UTF-8")
      dataset
    }
  ))
  package <- plan_validated_package(root)
  plan <- glc_collection_plan(package, variable_scope = "all")

  expect_identical(
    plan$groups$status,
    c(
      "included",
      "excluded",
      "excluded",
      "excluded",
      "excluded"
    )
  )
  expect_true("unsupported_format" %in% plan$groups$reason_codes[[2L]])
  expect_true("invalid_timezone" %in% plan$groups$reason_codes[[3L]])
  expect_true("incomplete_datetime" %in% plan$groups$reason_codes[[4L]])
  expect_true("no_declared_files" %in% plan$groups$reason_codes[[5L]])
  expect_true(all(vapply(
    plan$groups$messages,
    function(messages) {
      all(nzchar(messages))
    },
    logical(1)
  )))
  expect_explorer_plan_parity(package, plan)
  compatibility <- glcdp:::glc_explorer_selection_compatibility(
    glcdp:::glc_explorer_group_inventory(package),
    variable_names = character(),
    package = package
  )
  expect_false(compatibility$ok)
  expect_match(paste(compatibility$issues, collapse = " "), "measurement files")
})

test_that("planning resolves the IANA timezone catalog once per request", {
  package <- plan_validated_package(
    make_plan_matrix_fixture(list(identity, identity, identity))
  )
  calls <- 0L
  testthat::local_mocked_bindings(
    glc_plan_valid_timezones = function() {
      calls <<- calls + 1L
      base::OlsonNames()
    },
    .package = "glcdp"
  )

  expect_no_error(glc_collection_plan(package, variable_scope = "all"))
  expect_identical(calls, 1L)
})

test_that("reserved provenance declarations can never form a unit", {
  package <- plan_validated_package(make_reserved_plan_fixture())

  all <- glc_collection_plan(package, variable_scope = "all")
  selected <- glc_collection_plan(
    package,
    variable_scope = "selected",
    variables = "lux"
  )
  matched <- glc_collection_plan(
    package,
    terms = "photopic illuminance",
    variable_scope = "matched"
  )

  for (plan in list(all, selected, matched)) {
    expect_equal(nrow(plan$units), 0L)
    expect_identical(plan$groups$status, "excluded")
    expect_identical(
      plan$groups$reason_codes[[1L]],
      "reserved_provenance_column"
    )
    expect_match(plan$groups$messages[[1L]], ".glc_shadow", fixed = TRUE)
    expect_true(is.na(plan$preferred_unit_id))
  }
  expect_explorer_plan_parity(package, all)
  expect_explorer_plan_parity(
    package,
    selected,
    variable_names = "lux"
  )
  expect_explorer_plan_parity(
    package,
    matched,
    variable_terms = "photopic illuminance"
  )
  groups <- glcdp:::glc_explorer_group_inventory(package)
  all_compatibility <- glcdp:::glc_explorer_selection_compatibility(
    groups,
    variable_names = character(),
    package = package
  )
  selected_filter <- glcdp:::glc_explorer_filter_compatible_groups(
    groups,
    variable_names = "lux",
    package = package
  )
  matched_filter <- glcdp:::glc_explorer_filter_compatible_groups(
    groups,
    variable_terms = "photopic illuminance",
    package = package
  )
  expect_false(all_compatibility$ok)
  expect_match(
    paste(all_compatibility$issues, collapse = " "),
    "reserved for glcdp provenance",
    fixed = TRUE
  )
  expect_equal(selected_filter$included_count, 0L)
  expect_equal(matched_filter$included_count, 0L)

  expect_error(
    glc_read(package, dataset_id = "DS1"),
    regexp = "reserved provenance column",
    class = "rlang_error"
  )
})

test_that("standardization changes expected output but not partitioning", {
  package <- plan_validated_package(make_multi_dataset_fixture())
  lightlogr <- glc_collection_plan(
    package,
    variable_scope = "selected",
    variables = "lux",
    standardize = "lightlogr"
  )
  none <- glc_collection_plan(
    package,
    variable_scope = "selected",
    variables = "lux",
    standardize = "none"
  )

  expect_equal(nrow(lightlogr$units), nrow(none$units))
  expect_false(identical(lightlogr$units$unit_id, none$units$unit_id))
  expect_identical(
    lightlogr$compatibility_sets$compatibility_id,
    none$compatibility_sets$compatibility_id
  )
  expect_true(all(
    c(
      "Id",
      "file_group_id",
      "participant_Id",
      "Datetime",
      "file.name"
    ) %in%
      lightlogr$output_columns$name
  ))
  expect_true(all(
    c(
      ".glc_dataset_id",
      ".glc_file_group",
      ".glc_participant_id",
      ".glc_source_file",
      ".glc_datetime"
    ) %in%
      none$output_columns$name
  ))
  expect_true(all(!lightlogr$compatibility$standardize_affects_partition))
})

test_that("standard-column collisions stay subject to runtime validation", {
  package <- plan_validated_package(make_v3_contract_fixture())
  plan <- glc_collection_plan(
    package,
    variable_scope = "all",
    standardize = "lightlogr"
  )
  collisions <- plan$output_columns[
    plan$output_columns$collision_validation_required,
    ,
    drop = FALSE
  ]

  expect_setequal(collisions$name, c("Id", "Datetime", "file.name"))
  expect_true(all(collisions$runtime_validation_required))
  expect_true(all(grepl("glc_collect", collisions$message, fixed = TRUE)))
  expect_identical(
    collisions$expected_type[match(
      c("Id", "Datetime", "file.name"),
      collisions$name
    )],
    c("factor", "datetime", "string")
  )
  expect_identical(
    collisions$source_declared_type[match(
      c("Id", "Datetime", "file.name"),
      collisions$name
    )],
    c("string", "string", "string")
  )
})

test_that("unknown declaration metadata survives without live package state", {
  root <- make_glc_fixture("3.0.2")
  datasets <- fixture_read_datasets(root)
  datasets[[1L]]$future_dataset_field <- list(z = 2, a = 1)
  group <- datasets[[1L]]$dataset_file[[1L]]
  group$future_group_field <- list(beta = TRUE, alpha = "kept")
  group$dataset_file_variables[[2L]]$future_variable_field <- "kept"
  group$dataset_file_variables[[4L]][[
    "dataset_file_variables_factor_levels"
  ]][[1L]]$future_level_field <- 7
  datasets[[1L]]$dataset_file[[1L]] <- group
  fixture_write_datasets(root, datasets)

  plan <- glc_collection_plan(
    plan_validated_package(root),
    variable_scope = "all"
  )
  extension <- plan$extensions$metadata[[1L]]

  expect_identical(extension$dataset$future_dataset_field$a, 1L)
  expect_identical(extension$group$future_group_field$alpha, "kept")
  lux <- extension$variables[[which(vapply(
    extension$variables,
    function(variable) identical(variable$name, "lux"),
    logical(1)
  ))]]
  expect_identical(lux$fields$future_variable_field, "kept")
  quality <- extension$variables[[which(vapply(
    extension$variables,
    function(variable) identical(variable$name, "quality"),
    logical(1)
  ))]]
  expect_identical(
    quality$factor_levels[[1L]]$future_level_field,
    7L
  )
  expect_false(plan_has_live_value(plan))
})

test_that("measurement transport and content functions are never called", {
  root <- make_glc_fixture("3.0.2")
  package <- plan_validated_package(root)
  glcdp:::glc_model(package)
  glcdp:::glc_plan_metadata_load_resources(package)
  package$transport$file_inventory <- tibble::tibble(
    declared_path = "data/files/light.csv",
    expected_bytes = 999999
  )
  package$transport$file_info[["data/files/light.csv"]] <- list(
    expected_size = 999999
  )
  unlink(file.path(root, "data", "files", "light.csv"))
  forbidden <- function(...) {
    stop("forbidden measurement transport")
  }
  testthat::local_mocked_bindings(
    glc_files = forbidden,
    glc_summary = forbidden,
    glc_read = forbidden,
    glc_collect = forbidden,
    glc_materialize_file = forbidden,
    glc_file_info_internal = forbidden,
    glc_prefetch_file_info_internal = forbidden,
    .package = "glcdp"
  )

  expect_no_error(
    plan <- glc_collection_plan(
      package,
      terms = "photopic illuminance"
    )
  )
  expect_false(plan$files$bytes_known)
  expect_true(is.na(plan$files$declared_bytes))
  expect_equal(plan$compatibility_sets$known_file_count, 0L)
  expect_equal(plan$compatibility_sets$unknown_file_count, 1L)
  expect_equal(plan$compatibility_sets$declared_bytes, 0)
  expect_false(plan$compatibility_sets$declared_bytes_complete)
})

test_that("planner boundary validation is exact and actionable", {
  root <- make_glc_fixture("3.0.2")
  unverified <- glc_open(root, quiet = TRUE)
  expect_error(
    glc_collection_plan(
      unverified,
      terms = "photopic illuminance"
    ),
    "exact 40-character",
    class = "glcdp_collection_plan_revision"
  )

  package <- plan_validated_package(root)
  invalid_manifest <- package
  invalid_manifest$manifest$registry_verified <- FALSE
  expect_error(
    glc_collection_plan(
      invalid_manifest,
      terms = "photopic illuminance"
    ),
    "registry_verified",
    class = "glcdp_collection_plan_revision"
  )
  expect_error(
    glc_collection_plan(package, variable_scope = "matched"),
    "terms.*required"
  )
  expect_error(
    glc_collection_plan(
      package,
      variable_scope = "all",
      variables = "lux"
    ),
    "must be empty"
  )
  expect_error(
    glc_collection_plan(package, variable_scope = "selected"),
    "variables.*required"
  )
  expect_error(
    glc_collection_plan(
      package,
      variable_scope = "selected",
      variables = "missing"
    ),
    "Unknown declared source variable",
    class = "glcdp_unknown_variable"
  )
  expect_error(
    glc_collection_plan(
      package,
      terms = "photopic illuminance",
      dataset_id = "missing"
    ),
    "Unknown dataset",
    class = "glcdp_unknown_dataset"
  )
  expect_error(
    glc_collection_plan(
      package,
      terms = "photopic illuminance",
      file_group = "DS1:99"
    ),
    "Unknown stable file-group",
    class = "glcdp_unknown_file_group"
  )
  expect_error(
    glc_collection_plan(
      package,
      terms = "photopic illuminance",
      file_group = 1
    ),
    "character vector"
  )
  expect_error(
    glc_collection_plan(
      package,
      terms = c("photopic illuminance", "photopic illuminance")
    ),
    "must not contain duplicates"
  )

  remote <- plan_remote_package(package)
  expect_no_error(glc_collection_plan(
    remote,
    terms = "photopic illuminance"
  ))
  current_fail <- plan_remote_package(
    plan_validated_package(root),
    latest_pass_commit = strrep("b", 40L)
  )
  current_fail$verified <- TRUE
  expect_error(
    glc_collection_plan(
      current_fail,
      terms = "photopic illuminance"
    ),
    "latest passing",
    class = "glcdp_collection_plan_revision"
  )
})

test_that("duplicate source names and contradictory ids are fatal", {
  duplicate_variable <- plan_validated_package(make_glc_fixture("3.0.2"))
  model <- glcdp:::glc_model(duplicate_variable)
  model$datasets[[1L]]$groups[[1L]]$variables[[2L]]$name <- "timestamp"
  duplicate_variable$transport$model <- model
  expect_error(
    glc_collection_plan(
      duplicate_variable,
      variable_scope = "all"
    ),
    "duplicate source name",
    class = "glcdp_collection_plan_ambiguous_variable"
  )

  duplicate_group <- plan_validated_package(make_multi_dataset_fixture())
  model <- glcdp:::glc_model(duplicate_group)
  model$datasets[[2L]]$groups[[1L]]$id <- "DS1:1"
  duplicate_group$transport$model <- model
  expect_error(
    glc_collection_plan(
      duplicate_group,
      variable_scope = "all"
    ),
    "identifiers must be non-empty and unique",
    class = "glcdp_collection_plan_relationship"
  )
})

test_that("empty and partial metadata produce serializable exclusions", {
  root <- make_plan_matrix_fixture(list(
    identity,
    function(dataset) {
      dataset$dataset_file[[1L]]$dataset_file_variables <- list()
      dataset
    }
  ))
  plan <- glc_collection_plan(
    plan_validated_package(root),
    variable_scope = "all",
    dataset_id = "DS2"
  )

  expect_equal(nrow(plan$units), 0L)
  expect_true("variable_missing" %in% plan$groups$reason_codes[[2L]])
  expect_true("incomplete_datetime" %in% plan$groups$reason_codes[[2L]])
  expect_no_error(jsonlite::toJSON(
    plan,
    dataframe = "rows",
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  ))
})
