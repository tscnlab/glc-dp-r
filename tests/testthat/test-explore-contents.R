test_that("package contents helpers format and filter inventories", {
  fixture <- glc_open(make_multi_dataset_fixture(), quiet = TRUE)
  contents <- glcdp:::glc_explorer_load_contents(fixture)

  expect_equal(nrow(contents$datasets), 2L)
  expect_equal(nrow(contents$files), 2L)
  expect_equal(nrow(contents$variables), 8L)
  expect_gt(nrow(contents$metadata), 0L)
  expect_equal(
    unique(contents$metadata$record_id[contents$metadata$resource == "study"]),
    "S1"
  )
  expect_equal(
    unique(contents$metadata$record_id[
      contents$metadata$resource == "participants"
    ]),
    "P1"
  )
  expect_equal(
    unique(contents$metadata$record_id[
      contents$metadata$resource == "devices"
    ]),
    "D1"
  )
  expect_setequal(
    unique(contents$metadata$record_id[
      contents$metadata$resource == "datasets"
    ]),
    c("DS1", "DS2")
  )
  expect_equal(
    unique(contents$metadata$record_id[
      contents$metadata$resource == "device_datasheets"
    ]),
    "D1-sheet"
  )

  datasets <- glcdp:::glc_explorer_dataset_table(
    contents$datasets,
    contents$variables
  )
  expect_equal(datasets$Dataset, c("DS1", "DS2"))
  expect_equal(datasets$Variables, c(4L, 4L))
  expect_equal(datasets$Devices, c("D1", "D1"))

  file_groups <- glcdp:::glc_explorer_file_group_inventory(
    contents$files,
    contents$variables
  )
  expect_equal(file_groups$file_group_id, c("DS1:1", "DS2:1"))
  expect_equal(file_groups$variable_count, c(4L, 4L))
  expect_equal(
    file_groups$device_location,
    rep("non-dominant wrist", 2L)
  )
  expect_equal(file_groups$device_location_type, rep("body_worn", 2L))
  expect_equal(file_groups$modality_values, rep(list("light"), 2L))
  expect_equal(
    file_groups$variable_names,
    rep(list(c("lux", "quality", "timestamp", "worn")), 2L)
  )
  expect_equal(
    file_groups$variable_terms,
    rep(list(c("other", "photopic illuminance")), 2L)
  )
  choices <- glcdp:::glc_explorer_file_group_field_choices(file_groups)
  expect_equal(choices$device_id, "D1")
  expect_equal(choices$device_location, "non-dominant wrist")
  expect_equal(choices$location_type, "body_worn")
  expect_equal(choices$modality, "light")
  expect_equal(choices$role, "primary")
  expect_equal(choices$state, "raw")
  expect_equal(
    choices$variable,
    c("lux", "quality", "timestamp", "worn")
  )
  expect_equal(choices$term, c("other", "photopic illuminance"))
  filtered_groups <- glcdp:::glc_explorer_filter_file_groups(
    file_groups,
    dataset_ids = "DS2",
    query = "light"
  )
  expect_equal(filtered_groups$file_group_id, "DS2:1")
  all_groups <- glcdp:::glc_explorer_filter_file_groups(
    file_groups,
    dataset_ids = c("DS1", "DS2")
  )
  expect_equal(all_groups$file_group_id, c("DS1:1", "DS2:1"))
  expect_equal(
    nrow(glcdp:::glc_explorer_filter_file_groups(
      file_groups,
      dataset_ids = character()
    )),
    0L
  )
  empty_group_table <- glcdp:::glc_explorer_file_group_table(
    file_groups[FALSE, , drop = FALSE]
  )
  expect_equal(nrow(empty_group_table), 0L)
  expect_named(
    empty_group_table,
    names(glcdp:::glc_explorer_file_group_table(file_groups))
  )

  variables <- glcdp:::glc_explorer_filter_variables(
    contents$variables,
    dataset_ids = "DS2",
    query = "illuminance",
    primary_only = TRUE
  )
  expect_equal(variables$dataset_id, "DS2")
  expect_equal(variables$name, "lux")
  variables_both <- glcdp:::glc_explorer_filter_variables(
    contents$variables,
    dataset_ids = c("DS1", "DS2"),
    query = "illuminance",
    primary_only = TRUE
  )
  expect_equal(variables_both$dataset_id, c("DS1", "DS2"))

  metadata <- glcdp:::glc_explorer_filter_metadata(
    contents$metadata,
    resource = "participants",
    query = "P1"
  )
  expect_true(nrow(metadata) >= 1L)
  expect_true(all(metadata$resource == "participants"))
  metadata_table <- glcdp:::glc_explorer_metadata_table(metadata)
  expect_named(
    metadata_table,
    c("Resource", "Record ID", "Context", "Field", "Value")
  )
  expect_true(all(metadata_table$`Record ID` == "P1"))

  hierarchy <- glcdp:::glc_explorer_metadata_hierarchy_tag(metadata)
  hierarchy_html <- as.character(hierarchy)
  expect_match(hierarchy_html, "participants", fixed = TRUE)
  expect_match(
    hierarchy_html,
    "metadata_resource_body_resource_001",
    fixed = TRUE
  )
  expect_false(grepl("record 1 — P1", hierarchy_html, fixed = TRUE))
  expect_false(grepl("participant_internal_id", hierarchy_html, fixed = TRUE))
})

test_that("file-group field filters use exact values and combine with AND", {
  fixture <- glc_open(make_multi_dataset_fixture(), quiet = TRUE)
  contents <- glcdp:::glc_explorer_load_contents(fixture)
  groups <- glcdp:::glc_explorer_file_group_inventory(
    contents$files,
    contents$variables
  )
  groups$device_id[[2L]] <- "D2"
  groups$device_location[[2L]] <- "chest"
  groups$device_location_type[[2L]] <- "environment"
  groups$modalities[[2L]] <- "temperature"
  groups$modality_values[[2L]] <- "temperature"
  groups$role[[2L]] <- "supporting"
  groups$data_state[[2L]] <- "processed"
  groups$variable_names[[2L]] <- c("temperature", "timestamp")
  groups$variable_terms[[2L]] <- c("datetime", "temperature")

  expect_equal(
    glcdp:::glc_explorer_filter_file_groups(
      groups,
      device_ids = "D2"
    )$file_group_id,
    "DS2:1"
  )
  expect_equal(
    glcdp:::glc_explorer_filter_file_groups(
      groups,
      device_locations = "chest",
      location_types = "environment",
      modalities = "temperature",
      roles = "supporting",
      states = "processed",
      variable_names = "temperature",
      terms = "datetime"
    )$file_group_id,
    "DS2:1"
  )
  expect_equal(
    glcdp:::glc_explorer_filter_file_groups(
      groups,
      device_ids = c("D1", "D2")
    )$file_group_id,
    c("DS1:1", "DS2:1")
  )
  expect_equal(
    nrow(glcdp:::glc_explorer_filter_file_groups(
      groups,
      device_ids = "D1",
      device_locations = "chest"
    )),
    0L
  )
  expect_equal(
    glcdp:::glc_explorer_filter_file_groups(
      groups,
      query = "datetime"
    )$file_group_id,
    "DS2:1"
  )
  expect_equal(
    glcdp:::glc_explorer_filter_file_groups(
      groups,
      query = "non-dominant wrist"
    )$file_group_id,
    "DS1:1"
  )
  expect_equal(
    nrow(glcdp:::glc_explorer_filter_file_groups(
      groups,
      device_ids = "D"
    )),
    0L
  )
})

test_that("file-group compatibility covers the complete filtered ID set", {
  fixture <- glc_open(make_multi_dataset_fixture(), quiet = TRUE)
  groups <- glcdp:::glc_explorer_group_inventory(fixture)

  compatible <- glcdp:::glc_explorer_file_group_compatibility(
    groups,
    groups$file_group_id
  )
  expect_equal(compatible$state, "compatible")
  expect_true(compatible$ok)
  expect_equal(compatible$group_count, 2L)
  expect_equal(compatible$resolved_count, 2L)
  expect_equal(compatible$dataset_count, 2L)
  expect_length(compatible$issues, 0L)

  incompatible_groups <- groups
  incompatible_groups$timezone[[2L]] <- "UTC"
  incompatible <- glcdp:::glc_explorer_file_group_compatibility(
    incompatible_groups,
    incompatible_groups$file_group_id
  )
  expect_equal(incompatible$state, "incompatible")
  expect_false(incompatible$ok)
  expect_match(
    incompatible$issues,
    "different time zones",
    fixed = TRUE
  )

  unresolved <- glcdp:::glc_explorer_file_group_compatibility(
    groups,
    c("DS1:1", "missing:1")
  )
  expect_equal(unresolved$state, "incompatible")
  expect_equal(unresolved$group_count, 2L)
  expect_equal(unresolved$resolved_count, 1L)
  expect_equal(unresolved$unresolved_ids, "missing:1")
  expect_match(
    unresolved$issues,
    "not available in the package compatibility inventory",
    fixed = TRUE
  )

  empty <- glcdp:::glc_explorer_file_group_compatibility(
    groups,
    character()
  )
  expect_equal(empty$state, "zero")
  expect_false(empty$ok)
  expect_equal(empty$group_count, 0L)
  expect_length(empty$issues, 0L)

  large_groups <- groups[
    rep(1L, 150L),
    ,
    drop = FALSE
  ]
  large_groups$dataset_id <- sprintf("DS%03d", seq_len(150L))
  large_groups$file_group_id <- sprintf("DS%03d:1", seq_len(150L))
  large <- glcdp:::glc_explorer_file_group_compatibility(
    large_groups,
    large_groups$file_group_id
  )
  expect_equal(large$state, "compatible")
  expect_equal(large$group_count, 150L)
  expect_equal(large$resolved_count, 150L)
  expect_equal(large$dataset_count, 150L)
})

test_that("file-group handoff action is compact and explains refinement", {
  skip_if_not_installed("shiny")
  fixture <- glc_open(make_multi_dataset_fixture(), quiet = TRUE)
  groups <- glcdp:::glc_explorer_group_inventory(fixture)

  empty <- glcdp:::glc_explorer_file_group_compatibility(
    groups,
    character()
  )
  empty_html <- as.character(
    glcdp:::glc_explorer_file_group_compatibility_tag(
      empty,
      "file_group_handoff"
    )
  )
  expect_match(empty_html, 'role="status"', fixed = TRUE)
  expect_match(empty_html, 'aria-live="polite"', fixed = TRUE)
  expect_match(empty_html, "No matching file groups", fixed = TRUE)
  expect_false(grepl("file_group_handoff", empty_html, fixed = TRUE))

  compatible <- glcdp:::glc_explorer_file_group_compatibility(
    groups,
    groups$file_group_id
  )
  compatible_html <- as.character(
    glcdp:::glc_explorer_file_group_compatibility_tag(
      compatible,
      "file_group_handoff"
    )
  )
  expect_match(compatible_html, "2 groups ready for handoff", fixed = TRUE)
  expect_match(compatible_html, "Compatible selection", fixed = TRUE)
  expect_match(compatible_html, "btn-success", fixed = TRUE)
  expect_match(compatible_html, 'id="file_group_handoff"', fixed = TRUE)

  incompatible <- compatible
  incompatible$state <- "incompatible"
  incompatible$ok <- FALSE
  incompatible$issues <- sprintf("Issue %d", seq_len(8L))
  incompatible_html <- as.character(
    glcdp:::glc_explorer_file_group_compatibility_tag(
      incompatible,
      "file_group_handoff"
    )
  )
  expect_match(
    incompatible_html,
    "Want to import these files? Filter them first",
    fixed = TRUE
  )
  expect_match(incompatible_html, "Selection needs refinement", fixed = TRUE)
  expect_match(incompatible_html, "btn-warning", fixed = TRUE)
  expect_false(grepl("<li>", incompatible_html, fixed = TRUE))
  expect_match(incompatible_html, 'id="file_group_handoff"', fixed = TRUE)

  modal_html <- as.character(
    glcdp:::glc_explorer_file_group_compatibility_modal(incompatible)
  )
  issue_items <- regmatches(
    modal_html,
    gregexpr("<li>", modal_html, fixed = TRUE)
  )[[1L]]
  expect_length(issue_items, 6L)
  expect_match(modal_html, "Refine file groups before handoff", fixed = TRUE)
  expect_match(modal_html, "And 2 more issues.", fixed = TRUE)
  expect_match(modal_html, "until the button turns green", fixed = TRUE)
})

test_that("file-group navigation seeds only available dataset IDs", {
  datasets <- tibble::tibble(dataset_id = c("DS1", "DS2"))

  expect_equal(
    glcdp:::glc_explorer_file_group_navigation_selection(
      list(dataset_ids = c("DS2", "unknown", "DS2")),
      datasets
    ),
    "DS2"
  )
  expect_equal(
    glcdp:::glc_explorer_file_group_navigation_selection(
      list(dataset_ids = "unknown"),
      datasets
    ),
    "all"
  )
  expect_equal(
    glcdp:::glc_explorer_file_group_navigation_selection(
      list(),
      datasets
    ),
    "all"
  )
  expect_equal(
    glcdp:::glc_explorer_file_group_navigation_selection(
      list(dataset_ids = "DS1"),
      NULL
    ),
    "all"
  )
})

test_that("metadata paging exposes every matching value without a hard cap", {
  metadata <- tibble::tibble(
    resource = rep("participants", 1205L),
    record = seq_len(1205L),
    field = sprintf("participant_field_%04d", seq_len(1205L)),
    value = sprintf("value-%04d", seq_len(1205L)),
    context = sprintf("record %d", seq_len(1205L)),
    record_id = sprintf("P%04d", seq_len(1205L)),
    datasheet_path = NA_character_
  )

  first <- glcdp:::glc_explorer_metadata_page(
    metadata,
    page = 1L,
    page_size = 500L
  )
  last <- glcdp:::glc_explorer_metadata_page(
    metadata,
    page = 3L,
    page_size = 500L
  )

  expect_equal(nrow(first$data), 500L)
  expect_equal(first$total, 1205L)
  expect_equal(first$page_count, 3L)
  expect_equal(first$first, 1L)
  expect_equal(first$last, 500L)
  expect_equal(nrow(last$data), 205L)
  expect_equal(last$first, 1001L)
  expect_equal(last$last, 1205L)
  expect_equal(last$data$value[[205L]], "value-1205")
  expect_match(
    glcdp:::glc_explorer_metadata_page_message(last),
    "Showing 1,001\u20131,205 of 1,205",
    fixed = TRUE
  )
  expect_match(
    glcdp:::glc_explorer_metadata_page_message(last),
    "page 3 of 3",
    fixed = TRUE
  )

  clamped <- glcdp:::glc_explorer_metadata_page(
    metadata,
    page = 99L,
    page_size = 123L
  )
  expect_equal(clamped$page_size, 500L)
  expect_equal(clamped$page, 3L)

  pagination <- as.character(
    glcdp:::glc_explorer_metadata_pagination_tag(last)
  )
  expect_match(pagination, 'aria-label="Metadata pages"', fixed = TRUE)
  expect_match(pagination, "Page 3 of 3", fixed = TRUE)
  expect_match(pagination, "Previous", fixed = TRUE)
  expect_match(pagination, "Next", fixed = TRUE)
  expect_match(pagination, 'disabled="disabled"', fixed = TRUE)

  hierarchy <- as.character(
    glcdp:::glc_explorer_metadata_hierarchy_tag(metadata[1:501, ])
  )
  expect_match(hierarchy, "501 entries", fixed = TRUE)
  expect_false(grepl("value-0501", hierarchy, fixed = TRUE))
})

test_that("inventory paging keeps large file-group and variable tables responsive", {
  inventory <- tibble::tibble(
    id = sprintf("item-%03d", seq_len(205L))
  )

  first <- glcdp:::glc_explorer_inventory_page(inventory, page = 1L)
  last <- glcdp:::glc_explorer_inventory_page(inventory, page = 3L)

  expect_equal(nrow(first$data), 100L)
  expect_equal(first$first, 1L)
  expect_equal(first$last, 100L)
  expect_equal(nrow(last$data), 5L)
  expect_equal(last$data$id, sprintf("item-%03d", 201:205))
  expect_match(
    glcdp:::glc_explorer_inventory_page_message(
      last,
      "variable declarations"
    ),
    "Showing 201\u2013205 of 205 matching variable declarations; page 3 of 3.",
    fixed = TRUE
  )

  first_controls <- as.character(
    glcdp:::glc_explorer_inventory_pagination_tag(
      first,
      input_prefix = "variable",
      item = "variables"
    )
  )
  last_controls <- as.character(
    glcdp:::glc_explorer_inventory_pagination_tag(
      last,
      input_prefix = "variable",
      item = "variables"
    )
  )
  expect_match(first_controls, 'aria-label="Variables pages"', fixed = TRUE)
  expect_match(first_controls, 'id="variable_previous"', fixed = TRUE)
  expect_match(first_controls, 'id="variable_next"', fixed = TRUE)
  expect_match(first_controls, 'disabled="disabled"', fixed = TRUE)
  expect_match(last_controls, "Page 3 of 3", fixed = TRUE)
  expect_match(last_controls, 'disabled="disabled"', fixed = TRUE)
})

test_that("metadata IDs follow participant links and datasheet branches", {
  metadata <- tibble::tibble(
    resource = c(
      "participant_characteristics",
      "participant_characteristics",
      rep("device_datasheets", 10L)
    ),
    record = c(
      1L,
      1L,
      NA_integer_,
      NA_integer_,
      1L,
      1L,
      1L,
      NA_integer_,
      NA_integer_,
      1L,
      1L,
      1L
    ),
    field = c(
      "participant_internal_id",
      "participant_characteristic_name",
      "data/a.json.datasheet_id",
      "data/a.json.datasheet_model",
      "data/a.json.datasheet_channel.datasheet_channel_nr",
      "data/a.json.datasheet_channel.datasheet_channel_name",
      paste0(
        "data/a.json.datasheet_calibration_spectral_sensitivity.",
        "datasheet_calibration_spectral_sensitivity_wavelength"
      ),
      "data/b.json.datasheet_id",
      "data/b.json.datasheet_model",
      "data/b.json.datasheet_channel.datasheet_channel_nr",
      "data/b.json.datasheet_channel.datasheet_channel_name",
      paste0(
        "data/b.json.datasheet_calibration_spectral_sensitivity.",
        "datasheet_calibration_spectral_sensitivity_wavelength"
      )
    ),
    value = c(
      "P001",
      "Chronotype",
      "sheet-a",
      "A",
      "1",
      "lux",
      "450",
      "sheet-b",
      "B",
      "2",
      "irradiance",
      "500"
    ),
    context = c(
      "record 1",
      "record 1",
      "object",
      "object",
      "record 1",
      "record 1",
      "record 1",
      "object",
      "object",
      "record 1",
      "record 1",
      "record 1"
    )
  )
  metadata <- glcdp:::glc_explorer_add_metadata_record_ids(metadata)

  expect_equal(metadata$record_id[1:2], c("P001", "P001"))
  expect_equal(
    metadata$record_id[3:12],
    c(rep("sheet-a", 5L), rep("sheet-b", 5L))
  )
  expect_equal(
    metadata$datasheet_path[3:12],
    c(rep("data/a.json", 5L), rep("data/b.json", 5L))
  )

  participant_hierarchy <- metadata[
    metadata$resource == "participant_characteristics",
    ,
    drop = FALSE
  ]
  expect_equal(
    glcdp:::glc_explorer_metadata_record_label(participant_hierarchy),
    "record 1 — P001 — Chronotype"
  )
  participant_body <- as.character(
    glcdp:::glc_explorer_metadata_entry_body_tag(participant_hierarchy)
  )
  expect_match(participant_body, "participant_internal_id", fixed = TRUE)
  expect_match(participant_body, "Chronotype", fixed = TRUE)
  expect_false(grepl("<details", participant_body, fixed = TRUE))

  datasheets <- glcdp:::glc_explorer_filter_metadata(
    metadata,
    resource = "device_datasheets",
    query = "datasheet_model"
  )
  expect_equal(
    glcdp:::glc_explorer_metadata_table(datasheets)$`Record ID`,
    c("sheet-a", "sheet-b")
  )
  complete_datasheets <- glcdp:::glc_explorer_complete_metadata_entries(
    metadata,
    datasheets
  )
  datasheet_entries <- glcdp:::glc_explorer_metadata_entry_index(
    complete_datasheets
  )
  expect_equal(
    datasheet_entries$label,
    c("Datasheet — sheet-a", "Datasheet — sheet-b")
  )
  expect_match(
    datasheet_hierarchy <- as.character(
      glcdp:::glc_explorer_metadata_entry_body_tag(complete_datasheets)
    ),
    "Datasheet — sheet-a",
    fixed = TRUE
  )
  expect_match(datasheet_hierarchy, "Datasheet — sheet-b", fixed = TRUE)
  expect_match(datasheet_hierarchy, "Source: ", fixed = TRUE)
  expect_match(datasheet_hierarchy, "data/a.json", fixed = TRUE)
  expect_match(datasheet_hierarchy, "data/b.json", fixed = TRUE)
  expect_false(grepl("object —", datasheet_hierarchy, fixed = TRUE))

  all_datasheets <- metadata[
    metadata$resource == "device_datasheets",
    ,
    drop = FALSE
  ]
  hierarchy <- as.character(
    glcdp:::glc_explorer_metadata_entry_body_tag(all_datasheets)
  )
  expect_match(hierarchy, "Channels", fixed = TRUE)
  expect_match(hierarchy, "Channel 1 — lux", fixed = TRUE)
  expect_match(hierarchy, "Channel 2 — irradiance", fixed = TRUE)
  expect_match(hierarchy, "Calibration Spectral Sensitivity", fixed = TRUE)
  expect_match(
    hierarchy,
    "Calibration point 1 — wavelength: 450",
    fixed = TRUE
  )
  expect_false(grepl("record 1", hierarchy, fixed = TRUE))
})

test_that("nested metadata record arrays retain their associations", {
  simplify <- function(value) {
    jsonlite::fromJSON(
      jsonlite::toJSON(value, auto_unbox = TRUE, null = "null"),
      simplifyVector = TRUE
    )
  }
  study <- simplify(list(list(
    study_internal_id = "study-1",
    study_groups = list(
      list(
        study_group_name = "Group A",
        study_group_inclusion = c("A1", "A2")
      ),
      list(
        study_group_name = "Group B",
        study_group_inclusion = c("B1", "B2")
      )
    ),
    study_contributors = list(
      list(
        contributor_full_name = "Ada Example",
        contributor_roles = c("Conceptualization", "Data curation")
      ),
      list(
        contributor_full_name = "Bea Example",
        contributor_roles = c("Validation", "Writing")
      )
    )
  )))
  datasets <- simplify(list(list(
    dataset_internal_id = "DS1",
    dataset_variable_terms = list(
      list(term = "datetime", label = "Date and time"),
      list(term = "illuminance", label = "Illuminance")
    ),
    dataset_file = list(
      list(
        dataset_file_names = list("a.csv"),
        dataset_file_variables = list(
          list(
            dataset_file_variables_name = "quality",
            dataset_file_variables_type = "factor",
            dataset_file_variables_factor_levels = list(
              list(value = "good", label = "Good"),
              list(value = "bad", label = "Bad")
            )
          )
        )
      ),
      list(
        dataset_file_names = list("b.csv"),
        dataset_file_variables = list(
          list(
            dataset_file_variables_name = "lux",
            dataset_file_variables_type = "numeric"
          )
        )
      )
    )
  )))

  metadata <- glcdp:::glc_explorer_metadata_leaf_table(list(
    study = study,
    datasets = datasets
  ))
  metadata <- glcdp:::glc_explorer_add_metadata_record_ids(metadata)
  record_count <- function(collection) {
    paths <- metadata$.record_path[
      !is.na(metadata$.record_collection) &
        metadata$.record_collection == collection
    ]
    length(unique(paths))
  }

  expect_equal(record_count("study_groups"), 2L)
  expect_equal(record_count("study_contributors"), 2L)
  expect_equal(record_count("dataset_variable_terms"), 2L)
  expect_equal(record_count("dataset_file"), 2L)
  expect_equal(
    record_count("dataset_file.dataset_file_variables"),
    2L
  )
  expect_equal(
    record_count(paste0(
      "dataset_file.dataset_file_variables.",
      "dataset_file_variables_factor_levels"
    )),
    2L
  )

  contributor_names <- metadata[
    metadata$field == "study_contributors.contributor_full_name",
    ,
    drop = FALSE
  ]
  contributor_roles <- metadata[
    metadata$field == "study_contributors.contributor_roles",
    ,
    drop = FALSE
  ]
  ada_path <- contributor_names$.record_path[
    contributor_names$value == "Ada Example"
  ]
  bea_path <- contributor_names$.record_path[
    contributor_names$value == "Bea Example"
  ]
  expect_setequal(
    contributor_roles$value[contributor_roles$.record_path == ada_path],
    c("Conceptualization", "Data curation")
  )
  expect_setequal(
    contributor_roles$value[contributor_roles$.record_path == bea_path],
    c("Validation", "Writing")
  )

  study_html <- as.character(
    glcdp:::glc_explorer_metadata_entry_body_tag(
      metadata[metadata$resource == "study", , drop = FALSE]
    )
  )
  expect_match(study_html, "Contributor 1 — Ada Example", fixed = TRUE)
  expect_match(study_html, "Contributor 2 — Bea Example", fixed = TRUE)
  expect_match(study_html, "Study group 1 — Group A", fixed = TRUE)
  expect_match(study_html, "Study group 2 — Group B", fixed = TRUE)

  dataset_html <- as.character(
    glcdp:::glc_explorer_metadata_entry_body_tag(
      metadata[metadata$resource == "datasets", , drop = FALSE]
    )
  )
  expect_match(dataset_html, "Variable term 1 — datetime", fixed = TRUE)
  expect_match(dataset_html, "Variable term 2 — illuminance", fixed = TRUE)
  expect_match(dataset_html, "Dataset file 1 — a.csv", fixed = TRUE)
  expect_match(dataset_html, "Dataset file 2 — b.csv", fixed = TRUE)
  expect_match(dataset_html, "Variable 1 — quality", fixed = TRUE)
  expect_match(dataset_html, "Variable 1 — lux", fixed = TRUE)
  expect_match(dataset_html, "Factor level 1 — Good", fixed = TRUE)
  expect_match(dataset_html, "Factor level 2 — Bad", fixed = TRUE)
})

test_that("singleton flat child records render inline without a disclosure", {
  datasets <- list(list(
    dataset_internal_id = "DS1",
    dataset_file = list(list(
      dataset_file_names = list("a.csv"),
      dataset_file_variables = list(list(
        dataset_file_variables_name = "Id",
        dataset_file_variables_type = "string",
        dataset_file_variables_term = list(
          variable_term = "participant_id"
        )
      ))
    ))
  ))
  metadata <- glcdp:::glc_explorer_metadata_leaf_table(list(
    datasets = datasets
  ))
  metadata <- glcdp:::glc_explorer_add_metadata_record_ids(metadata)
  html <- as.character(
    glcdp:::glc_explorer_metadata_entry_body_tag(metadata)
  )

  expect_match(html, "metadata-inline-child", fixed = TRUE)
  expect_match(
    html,
    "dataset_file_variables_term \u203a variable_term",
    fixed = TRUE
  )
  expect_match(html, "participant_id", fixed = TRUE)
  expect_false(grepl(
    "Variable term 1 \u2014 participant_id",
    html,
    fixed = TRUE
  ))
})

test_that("device and datasheet records use the stable 3.0 hierarchy", {
  simplify <- function(value) {
    jsonlite::fromJSON(
      jsonlite::toJSON(value, auto_unbox = TRUE, null = "null"),
      simplifyVector = TRUE
    )
  }
  devices <- simplify(list(list(
    schema_version = "3.0.2",
    device_internal_id = "device-1",
    device_sensors = list(
      list(device_sensor_type = "photopic light sensor"),
      list(device_sensor_type = "triaxial accelerometer")
    )
  )))
  datasheets <- simplify(list(list(
    schema_version = "3.0.2",
    datasheet_id = "sheet-1",
    datasheet_sensor_modality = c("light", "accelerometer"),
    datasheet_calibration_parameters = list(
      list(
        parameter_name = "gain",
        parameter_value = "1.0",
        parameter_unit = "ratio"
      ),
      list(
        parameter_name = "offset",
        parameter_value = "0.1",
        parameter_unit = "ratio"
      )
    ),
    datasheet_channel = list(
      list(
        datasheet_channel_nr = 1,
        datasheet_channel_name = "lux",
        datasheet_channel_unit = "lx"
      ),
      list(
        datasheet_channel_nr = 2,
        datasheet_channel_name = "activity",
        datasheet_channel_unit = "counts"
      )
    )
  )))
  metadata <- glcdp:::glc_explorer_metadata_leaf_table(list(
    devices = devices,
    device_datasheets = datasheets
  ))
  metadata <- glcdp:::glc_explorer_add_metadata_record_ids(metadata)
  record_count <- function(collection) {
    paths <- metadata$.record_path[
      !is.na(metadata$.record_collection) &
        metadata$.record_collection == collection
    ]
    length(unique(paths))
  }

  expect_equal(record_count("device_sensors"), 2L)
  expect_equal(record_count("datasheet_calibration_parameters"), 2L)
  expect_equal(record_count("datasheet_channel"), 2L)

  device_html <- as.character(
    glcdp:::glc_explorer_metadata_entry_body_tag(
      metadata[metadata$resource == "devices", , drop = FALSE]
    )
  )
  expect_match(
    device_html,
    "Sensor 1 — photopic light sensor",
    fixed = TRUE
  )
  expect_match(
    device_html,
    "Sensor 2 — triaxial accelerometer",
    fixed = TRUE
  )

  datasheet_html <- as.character(
    glcdp:::glc_explorer_metadata_entry_body_tag(
      metadata[metadata$resource == "device_datasheets", , drop = FALSE]
    )
  )
  expect_equal(
    lengths(regmatches(
      datasheet_html,
      gregexpr("Datasheet — sheet-1", datasheet_html, fixed = TRUE)
    ))[[1L]],
    1L
  )
  expect_equal(
    lengths(regmatches(
      datasheet_html,
      gregexpr("Source:", datasheet_html, fixed = TRUE)
    ))[[1L]],
    1L
  )
  expect_match(datasheet_html, "Calibration Parameters", fixed = TRUE)
  expect_match(
    datasheet_html,
    "Calibration parameter 1 — gain",
    fixed = TRUE
  )
  expect_match(
    datasheet_html,
    "Calibration parameter 2 — offset",
    fixed = TRUE
  )
  expect_match(datasheet_html, "Channels", fixed = TRUE)
  expect_match(datasheet_html, "Channel 1 — lux", fixed = TRUE)
  expect_match(datasheet_html, "Channel 2 — activity", fixed = TRUE)
})

test_that("metadata hierarchy loads complete compact records in batches", {
  record_count <- 30L
  fields <- c(
    "participant_internal_id",
    "participant_characteristic_name",
    "participant_characteristic_value",
    "participant_characteristic_unit",
    "participant_characteristic_description"
  )
  metadata <- tibble::tibble(
    resource = "participant_characteristics",
    record = rep(seq_len(record_count), each = length(fields)),
    field = rep(fields, record_count),
    value = unlist(lapply(seq_len(record_count), function(record) {
      c(
        sprintf("P%03d", record),
        if (record == 1L) "VLSQ8" else sprintf("Characteristic %d", record),
        as.character(record + 34L),
        "score",
        sprintf("Description %d", record)
      )
    })),
    context = paste("record", rep(seq_len(record_count), each = length(fields)))
  )
  metadata <- glcdp:::glc_explorer_add_metadata_record_ids(metadata)
  metadata <- glcdp:::glc_explorer_add_metadata_hierarchy_ids(metadata)

  entries <- glcdp:::glc_explorer_metadata_entry_index(metadata)
  expect_equal(nrow(entries), record_count)
  expect_equal(entries$label[[1L]], "record 1 — P001 — VLSQ8")
  expect_true(all(entries$value_count == 5L))
  expect_true(all(entries$is_flat))

  first_batch <- glcdp:::glc_explorer_metadata_entry_slice(
    metadata,
    limit = 25L
  )
  expect_equal(first_batch$loaded_entry_count, 25L)
  expect_equal(first_batch$loaded_value_count, 125L)
  expect_equal(first_batch$remaining_entry_count, 5L)
  expect_equal(length(unique(first_batch$data$.entry_id)), 25L)

  shell <- as.character(
    glcdp:::glc_explorer_metadata_hierarchy_tag(metadata)
  )
  expect_match(shell, "30 entries · 150 values", fixed = TRUE)
  expect_match(shell, "metadata-lazy-placeholder", fixed = TRUE)
  expect_match(shell, "Loading 30 complete metadata entries", fixed = TRUE)
  expect_false(grepl("record 1 — P001 — VLSQ8", shell, fixed = TRUE))
  expect_false(grepl("Description 1", shell, fixed = TRUE))

  batch <- as.character(
    glcdp:::glc_explorer_metadata_resource_body_tag(first_batch)
  )
  expect_match(batch, "record 1 — P001 — VLSQ8", fixed = TRUE)
  expect_match(batch, "Load next 5 entries", fixed = TRUE)
  expect_match(batch, "fa-rectangle-list", fixed = TRUE)
  expect_match(batch, "metadata-lazy-placeholder", fixed = TRUE)
  expect_match(batch, "Loading 5 metadata values", fixed = TRUE)
  expect_false(grepl("fa-id-card", batch, fixed = TRUE))
  expect_false(grepl("Description 1", batch, fixed = TRUE))

  first_record <- metadata[metadata$record == 1L, , drop = FALSE]
  body <- as.character(
    glcdp:::glc_explorer_metadata_entry_body_tag(first_record)
  )
  expect_equal(lengths(regmatches(body, gregexpr("<dt", body)))[[1L]], 5L)
  expect_match(body, "Description 1", fixed = TRUE)
  expect_false(grepl("<details", body, fixed = TRUE))

  singleton <- glcdp:::glc_explorer_metadata_entry_slice(first_record)
  singleton_body <- as.character(
    glcdp:::glc_explorer_metadata_resource_body_tag(singleton)
  )
  expect_match(singleton_body, "Description 1", fixed = TRUE)
  expect_false(grepl("metadata-entry-hierarchy", singleton_body, fixed = TRUE))

  matches <- glcdp:::glc_explorer_filter_metadata(metadata, query = "35")
  expanded <- glcdp:::glc_explorer_complete_metadata_entries(
    metadata,
    matches
  )
  expect_equal(nrow(matches), 1L)
  expect_equal(nrow(expanded), 5L)
  expect_equal(
    glcdp:::glc_explorer_metadata_record_label(expanded),
    "record 1 — P001 — VLSQ8"
  )
})

test_that("repeated metadata values stay within their parent record", {
  study <- jsonlite::fromJSON(
    jsonlite::toJSON(
      list(list(
        study_internal_id = "study-1",
        study_keywords = c("light", "sleep"),
        study_groups = list(list(
          study_group_name = "IZTECH participants",
          study_group_inclusion = c(
            "Adults aged 18–65",
            "Employed at least 80%"
          ),
          study_group_exclusion = c(
            "Psychiatric disorders",
            "Sleep disorders",
            "Shift work"
          ),
          study_group_datasets = sprintf("MELIDOS_IZTECH_S%03d", 1:4)
        ))
      )),
      auto_unbox = TRUE
    ),
    simplifyVector = TRUE
  )
  metadata <- glcdp:::glc_explorer_metadata_leaf_table(list(study = study))
  metadata <- glcdp:::glc_explorer_add_metadata_record_ids(metadata)

  html <- as.character(
    glcdp:::glc_explorer_metadata_entry_body_tag(metadata)
  )
  repeated <- regmatches(
    html,
    gregexpr('class="metadata-repeated-field mb-2"', html, fixed = TRUE)
  )[[1L]]

  expect_length(repeated, 4L)
  expect_match(html, "study_keywords", fixed = TRUE)
  expect_match(html, "Study Groups", fixed = TRUE)
  expect_match(html, "Study group 1 — IZTECH participants", fixed = TRUE)
  expect_match(html, "study_group_inclusion", fixed = TRUE)
  expect_match(html, "2 items", fixed = TRUE)
  expect_match(html, "study_group_exclusion", fixed = TRUE)
  expect_match(html, "3 items", fixed = TRUE)
  expect_match(html, "study_group_datasets", fixed = TRUE)
  expect_match(html, "4 items", fixed = TRUE)
  expect_match(html, "MELIDOS_IZTECH_S004", fixed = TRUE)
  expect_match(html, "metadata-values-grid", fixed = TRUE)
  expect_equal(
    lengths(regmatches(
      html,
      gregexpr("metadata-disclosure-arrow", html, fixed = TRUE)
    ))[[1L]],
    4L
  )
  expect_false(grepl("col-sm-5", html, fixed = TRUE))
})

test_that("package contents UI offers multi-dataset selectize controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  html <- as.character(glcdp:::package_contents_ui("contents"))
  multiple <- regmatches(
    html,
    gregexpr('multiple="multiple"', html, fixed = TRUE)
  )[[1L]]

  expect_length(multiple, 10L)
  expect_match(html, "All datasets", fixed = TRUE)
  expect_match(html, "contents-file_group_select_all", fixed = TRUE)
  expect_match(html, "contents-variable_select_all", fixed = TRUE)
  expect_match(html, "contents-file_group_clear", fixed = TRUE)
  expect_match(html, "contents-variable_clear", fixed = TRUE)
  expect_match(html, "contents-metadata_page_size", fixed = TRUE)
  expect_match(html, "contents-metadata_page", fixed = TRUE)
  expect_match(html, "contents-metadata_pagination", fixed = TRUE)
  expect_match(html, "contents-file_group_compatibility", fixed = TRUE)
  expect_match(html, "contents-file_group_pagination", fixed = TRUE)
  expect_match(html, "contents-variable_pagination", fixed = TRUE)
  expect_match(html, "Values per page", fixed = TRUE)
  expect_match(html, "contents-file_group_device_id", fixed = TRUE)
  expect_match(html, "contents-file_group_device_location", fixed = TRUE)
  expect_match(html, "contents-file_group_location_type", fixed = TRUE)
  expect_match(html, "contents-file_group_modality", fixed = TRUE)
  expect_match(html, "contents-file_group_role", fixed = TRUE)
  expect_match(html, "contents-file_group_state", fixed = TRUE)
  expect_match(html, "contents-file_group_variable", fixed = TRUE)
  expect_match(html, "contents-file_group_term", fixed = TRUE)
  expect_match(html, "Contained variables", fixed = TRUE)
  expect_match(html, "Semantic terms", fixed = TRUE)
  expect_match(html, 'placeholder="Search datasets"', fixed = TRUE)
  expect_match(
    html,
    paste(
      "Search by dataset ID, study ID, participant ID,",
      "device ID, or modality."
    ),
    fixed = TRUE
  )
  expect_false(grepl(">Filters<", html, fixed = TRUE))
  tab_positions <- vapply(
    c("Metadata", "Datasets", "Variables", "File groups"),
    function(tab)
      regexpr(
        paste0('data-value="', tab, '"'),
        html,
        fixed = TRUE
      )[[1L]],
    integer(1)
  )
  expect_true(all(tab_positions > 0L))
  expect_true(all(diff(tab_positions) > 0L))
  expect_match(
    html,
    paste(
      "Hierarchy loads complete records as you open them.",
      "Use Table to compare matching values across records."
    ),
    fixed = TRUE
  )
  expect_match(
    html,
    "input.metadata_view === &#39;table&#39;",
    fixed = TRUE
  )
  expect_match(html, 'role="note"', fixed = TRUE)
  expect_false(grepl("calc(100vh", html, fixed = TRUE))
  expect_false(grepl("height:100%", html, fixed = TRUE))
})

test_that("package contents load lazily and expose filtered results", {
  skip_if_not_installed("shiny")
  fixture <- glc_open(make_multi_dataset_fixture(), quiet = TRUE)
  active <- shiny::reactiveVal(FALSE)
  navigation <- shiny::reactiveVal(NULL)
  pending_load <- NULL
  load_count <- 0L
  selected_tab <- NULL
  loader <- function(package) {
    load_count <<- load_count + 1L
    glcdp:::glc_explorer_load_contents(package)
  }
  scheduler <- function(callback, session) {
    pending_load <<- callback
  }

  shiny::testServer(
    glcdp:::package_contents_server,
    args = list(
      package = shiny::reactive(fixture),
      active = active,
      navigation = navigation,
      load_contents = loader,
      schedule_after_flush = scheduler,
      select_nav = function(id, selected, session) {
        selected_tab <<- selected
      }
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      expect_null(state$contents())
      expect_equal(state$status()$state, "ready")
      expect_equal(load_count, 0L)

      active(TRUE)
      session$flushReact()
      expect_equal(state$status()$state, "loading")
      expect_true(is.function(pending_load))

      pending_load()
      session$flushReact()
      expect_equal(state$status()$state, "success")
      expect_equal(load_count, 1L)
      expect_equal(nrow(state$contents()$datasets), 2L)
      session$setInputs(
        file_group_dataset_id = "all",
        variable_dataset_id = "all"
      )
      expect_equal(nrow(state$file_groups()), 2L)
      expect_equal(nrow(state$compatibility_groups()), 2L)
      expect_equal(state$file_group_compatibility()$state, "compatible")
      expect_equal(state$file_group_compatibility()$group_count, 2L)
      expect_equal(
        state$file_group_compatibility()$group_count,
        nrow(state$file_groups())
      )

      session$setInputs(
        file_group_dataset_id = "DS2",
        file_group_query = "light",
        file_group_device_id = "D1",
        file_group_device_location = "non-dominant wrist",
        file_group_location_type = "body_worn",
        file_group_modality = "light",
        file_group_role = "primary",
        file_group_state = "raw",
        file_group_variable = "lux",
        file_group_term = "photopic illuminance",
        variable_dataset_id = "DS2",
        variable_query = "quality",
        primary_only = FALSE,
        metadata_resource = "participants",
        metadata_query = "P1"
      )
      expect_equal(state$file_groups()$file_group_id, "DS2:1")
      expect_equal(state$file_group_compatibility()$state, "compatible")
      expect_equal(state$file_group_compatibility()$group_count, 1L)
      expect_equal(state$variables()$dataset_id, "DS2")
      expect_equal(state$variables()$name, "quality")
      expect_true(all(state$metadata()$resource == "participants"))

      session$setInputs(file_group_handoff = 1L)
      handoff <- state$handoff_request()
      expect_named(
        handoff,
        c("package_key", "request_id", "dataset_ids", "file_group_ids")
      )
      expect_equal(
        handoff$package_key,
        glcdp:::glc_explorer_package_key(fixture)
      )
      expect_equal(handoff$request_id, 1L)
      expect_equal(handoff$dataset_ids, "DS2")
      expect_equal(handoff$file_group_ids, "DS2:1")

      session$setInputs(file_group_term = "temperature")
      expect_equal(nrow(state$file_groups()), 0L)
      expect_equal(state$file_group_compatibility()$state, "zero")
      expect_identical(state$handoff_request(), handoff)
      session$setInputs(file_group_term = character())

      session$setInputs(
        file_group_dataset_id = "all",
        file_group_query = "",
        file_group_device_id = character(),
        file_group_device_location = character(),
        file_group_location_type = character(),
        file_group_modality = character(),
        file_group_role = character(),
        file_group_state = character(),
        file_group_variable = character(),
        file_group_term = character()
      )
      session$setInputs(file_group_handoff = 2L)
      expect_equal(state$handoff_request()$request_id, 2L)
      expect_equal(state$handoff_request()$dataset_ids, c("DS1", "DS2"))
      expect_equal(
        state$handoff_request()$file_group_ids,
        c("DS1:1", "DS2:1")
      )

      navigation(list(
        tab = "Metadata",
        metadata_resource = "study",
        request_id = 1L
      ))
      session$flushReact()
      expect_equal(selected_tab, "Metadata")

      active(FALSE)
      active(TRUE)
      session$flushReact()
      expect_equal(load_count, 1L)
    }
  )
})

test_that("package contents server expands metadata by complete records", {
  skip_if_not_installed("shiny")
  fixture <- glc_open(make_multi_dataset_fixture(), quiet = TRUE)
  hierarchy_contents <- glcdp:::glc_explorer_load_contents(fixture)
  fields <- c(
    "participant_internal_id",
    "participant_characteristic_name",
    "participant_characteristic_value",
    "participant_characteristic_unit",
    "participant_characteristic_description"
  )
  hierarchy_contents$metadata <- tibble::tibble(
    resource = "participant_characteristics",
    record = rep(seq_len(30L), each = length(fields)),
    field = rep(fields, 30L),
    value = unlist(lapply(seq_len(30L), function(record) {
      c(
        sprintf("P%03d", record),
        if (record == 1L) "VLSQ8" else sprintf("Characteristic %d", record),
        as.character(record + 34L),
        "score",
        sprintf("Description %d", record)
      )
    })),
    context = paste("record", rep(seq_len(30L), each = length(fields)))
  )
  hierarchy_contents$metadata <- glcdp:::glc_explorer_add_metadata_record_ids(
    hierarchy_contents$metadata
  )

  shiny::testServer(
    glcdp:::package_contents_server,
    args = list(
      package = shiny::reactive(fixture),
      active = shiny::reactive(TRUE),
      navigation = shiny::reactive(NULL),
      load_contents = function(package) hierarchy_contents,
      schedule_after_flush = function(callback, session) callback()
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      session$setInputs(
        contents_tab = "Metadata",
        metadata_resource = "participant_characteristics",
        metadata_query = "",
        metadata_view = "hierarchy"
      )
      session$flushReact()

      expect_equal(state$hierarchy_resources()$entry_count, 30L)
      expect_equal(state$hierarchy_resources()$value_count, 150L)
      expect_equal(nrow(state$visible_hierarchy_entries()), 0L)
      expect_match(
        output$metadata_count,
        "150 matching metadata value(s) in 30 complete entries",
        fixed = TRUE
      )

      session$setInputs(metadata_resources = "resource_001")
      session$flushReact()
      expect_equal(nrow(state$visible_hierarchy_entries()), 25L)
      expect_equal(
        state$visible_hierarchy_entries()$label[[1L]],
        "record 1 — P001 — VLSQ8"
      )
      first_entry <- state$visible_hierarchy_entries()$entry_id[[1L]]
      session$setInputs(
        metadata_records_resource_001 = first_entry
      )
      session$flushReact()
      body_id <- paste0("metadata_entry_body_", first_entry)
      expect_match(
        output[[body_id]]$html,
        "participant_internal_id",
        fixed = TRUE
      )

      session$setInputs(contents_tab = "Variables")
      session$flushReact()
      expect_null(output[[body_id]]$html)
      session$setInputs(contents_tab = "Metadata")
      session$flushReact()

      session$setInputs(metadata_more_resource_001 = 1L)
      session$flushReact()
      expect_equal(nrow(state$visible_hierarchy_entries()), 30L)

      session$setInputs(metadata_query = "35")
      session$flushReact()
      expect_equal(nrow(state$metadata()), 1L)
      expect_equal(nrow(state$hierarchy_metadata()), 5L)
      expect_equal(nrow(state$visible_hierarchy_entries()), 1L)
      expect_equal(
        state$visible_hierarchy_entries()$label,
        "record 1 — P001 — VLSQ8"
      )
    }
  )
})

test_that("package contents server pages the complete metadata result", {
  skip_if_not_installed("shiny")
  fixture <- glc_open(make_multi_dataset_fixture(), quiet = TRUE)
  large_contents <- glcdp:::glc_explorer_load_contents(fixture)
  large_contents$metadata <- tibble::tibble(
    resource = rep("participants", 1205L),
    record = seq_len(1205L),
    field = sprintf("participant_field_%04d", seq_len(1205L)),
    value = sprintf("value-%04d", seq_len(1205L)),
    context = sprintf("record %d", seq_len(1205L)),
    record_id = sprintf("P%04d", seq_len(1205L)),
    datasheet_path = NA_character_
  )

  shiny::testServer(
    glcdp:::package_contents_server,
    args = list(
      package = shiny::reactive(fixture),
      active = shiny::reactive(TRUE),
      navigation = shiny::reactive(NULL),
      load_contents = function(package) large_contents,
      schedule_after_flush = function(callback, session) callback()
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      session$setInputs(
        contents_tab = "Metadata",
        metadata_resource = "all",
        metadata_query = "",
        metadata_view = "table",
        metadata_page_size = "500",
        metadata_page = 3L
      )
      session$flushReact()

      expect_equal(nrow(state$metadata()), 1205L)
      expect_equal(state$metadata_page()$page, 3L)
      expect_equal(state$metadata_page()$page_count, 3L)
      expect_equal(nrow(state$metadata_page()$data), 205L)
      expect_equal(
        state$metadata_page()$data$value[[205L]],
        "value-1205"
      )
      expect_match(
        output$metadata_count,
        "Showing 1,001\u20131,205 of 1,205",
        fixed = TRUE
      )
    }
  )
})

test_that("package contents report loading failures", {
  skip_if_not_installed("shiny")
  fixture <- glc_open(make_glc_fixture(), quiet = TRUE)

  shiny::testServer(
    glcdp:::package_contents_server,
    args = list(
      package = shiny::reactive(fixture),
      active = shiny::reactive(TRUE),
      navigation = shiny::reactive(NULL),
      load_contents = function(package) stop("contents failed"),
      schedule_after_flush = function(callback, session) callback()
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      expect_null(state$contents())
      expect_equal(state$status()$state, "error")
      expect_match(state$status()$message, "contents failed", fixed = TRUE)
    }
  )
})

test_that("package contents showcase creates a Shiny application", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  fixture <- glc_open(make_glc_fixture(), quiet = TRUE)
  expect_s3_class(glcdp:::package_contents_app(fixture), "shiny.appobj")
})
