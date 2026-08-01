selection_facets <- function() {
  list(
    participant = list(
      age = character(),
      sex = character(),
      gender = character(),
      characteristic_name = character(),
      characteristic_values = character()
    ),
    device = list(
      manufacturer = character(),
      model = character(),
      sensor_type = character()
    )
  )
}

selection_group <- function(
  dataset_id,
  file_group_id,
  device_id,
  device_location = "non-dominant wrist",
  device_location_type = "body_worn",
  modalities = "light",
  role = "primary",
  data_state = "raw",
  variables = tibble::tibble(
    name = c("timestamp", "lux"),
    type = c("string", "numeric"),
    term = c("datetime", "photopic_illuminance"),
    term_name = c("Date and time", "Photopic illuminance"),
    primary = c(FALSE, TRUE)
  )
) {
  tibble::tibble(
    dataset_id = dataset_id,
    file_group = 1L,
    file_group_id = file_group_id,
    device_id = device_id,
    device_location = device_location,
    device_location_type = device_location_type,
    format = "csv",
    timezone = "Europe/Berlin",
    modalities = list(modalities),
    role = role,
    data_state = data_state,
    datetime_source = "column",
    datetime_date = "timestamp",
    datetime_format = "YYYY-MM-DD HH:mm:ss",
    datetime_time = NA_character_,
    datetime_time_format = NA_character_,
    variables = list(variables)
  )
}

selection_data <- function() {
  groups <- dplyr::bind_rows(
    selection_group("DS1", "DS1:1", "D1"),
    selection_group("DS2", "DS2:1", "D2"),
    selection_group("DS3", "DS3:1", "D1")
  )
  variables <- dplyr::bind_rows(lapply(seq_len(nrow(groups)), function(index) {
    value <- groups$variables[[index]]
    tibble::tibble(
      dataset_id = groups$dataset_id[[index]],
      file_group_id = groups$file_group_id[[index]],
      name = value$name,
      term = value$term,
      term_name = value$term_name,
      primary = value$primary
    )
  }))
  files <- tibble::tibble(
    dataset_id = groups$dataset_id,
    file_group_id = groups$file_group_id,
    device_id = groups$device_id,
    declared_path = paste0("data/", groups$dataset_id, ".csv"),
    expected_bytes = c(100, 200, 300)
  )
  list(
    participants = tibble::tibble(
      participant_id = c("P1", "P2", "P3", "P4"),
      age = c("29", "29", "40", NA_character_),
      sex = c("female", "male", "female", "female"),
      gender = c("woman", "man", "woman", NA_character_)
    ),
    participant_characteristics = tibble::tibble(
      participant_id = rep(c("P1", "P2", "P3", "P4"), 2L),
      characteristic_name = rep(c("Chronotype", "MEQ"), each = 4L),
      characteristic_value = c(
        "morning",
        "morning",
        "evening",
        "evening",
        "35",
        "40",
        "55",
        "60"
      )
    ),
    devices = tibble::tibble(
      device_id = c("D1", "D2"),
      manufacturer = c("Acme", "Acme"),
      model = c("One", "Two"),
      sensor_type = c("light", "light")
    ),
    datasets = tibble::tibble(
      dataset_id = c("DS1", "DS2", "DS3"),
      participant_id = c("P1", "P2", "P3")
    ),
    groups = groups,
    variables = variables,
    files = files,
    metadata_issues = character()
  )
}

selection_package <- function() {
  commit <- paste(rep("a", 40L), collapse = "")
  structure(
    list(
      source_type = "remote",
      repo = "owner/example-data",
      commit = commit,
      descriptor = list(name = "example-data"),
      registry_row = tibble::tibble(
        id = "example-data",
        latest_pass_commit = commit,
        registry_generated_at = "2026-07-20T12:00:00Z"
      )
    ),
    class = "glc_package"
  )
}

metadata_selection_package <- function() {
  package <- selection_package()
  package$descriptor$resources <- list(
    list(
      name = "study",
      path = "metadata/study.json",
      format = "json"
    ),
    list(
      name = "datasets",
      path = "metadata/datasets.json",
      format = "json"
    ),
    list(
      name = "devices",
      path = "metadata/devices.json",
      format = "json"
    )
  )
  package
}

test_that("selection metadata normalization supports JSON and tables", {
  json_metadata <- list(
    participants = list(
      list(
        participant_internal_id = "P1",
        participant_age = 29,
        participant_sex = "female",
        participant_gender = "woman"
      ),
      list(
        participant_internal_id = "P2",
        participant_age = 35,
        participant_sex = "male"
      )
    ),
    participant_characteristics = list(
      list(
        participant_internal_id = "P1",
        participant_characteristic_name = "Chronotype",
        participant_characteristic_values = c("morning", "evening")
      )
    ),
    devices = list(list(
      device_internal_id = "D1",
      device_manufacturer = "Acme",
      device_model = "One",
      device_sensor_type = "light"
    ))
  )
  json <- glcdp:::glc_explorer_normalize_selection_metadata(json_metadata)

  expect_equal(json$participants$participant_id, c("P1", "P2"))
  expect_equal(json$participants$age, c("29", "35"))
  expect_equal(json$devices$manufacturer, "Acme")
  expect_setequal(
    json$participant_characteristics$characteristic_value,
    c("morning", "evening")
  )

  tabular_metadata <- list(
    participants = data.frame(
      participant_internal_id = c("P1", "P2"),
      participant_age = c(29, 35),
      participant_sex = c("female", "male"),
      participant_gender = c("woman", NA_character_)
    ),
    participant_characteristics = data.frame(
      participant_internal_id = "P1",
      participant_characteristic_name = "Chronotype",
      participant_characteristic_value = "morning"
    ),
    devices = data.frame(
      device_internal_id = "D1",
      device_manufacturer = "Acme",
      device_model = "One",
      sensor_type = "light"
    )
  )
  tabular <- glcdp:::glc_explorer_normalize_selection_metadata(
    tabular_metadata
  )

  expect_equal(tabular$participants$participant_id, c("P1", "P2"))
  expect_equal(tabular$participants$gender, c("woman", NA_character_))
  expect_equal(
    tabular$participant_characteristics$characteristic_name,
    "Chronotype"
  )
  expect_equal(tabular$devices$sensor_type, "light")
})

test_that("missing optional metadata fields remain inactive", {
  metadata <- list(
    participants = list(list(participant_internal_id = "P1")),
    devices = data.frame(device_internal_id = "D1")
  )
  normalized <- glcdp:::glc_explorer_normalize_selection_metadata(metadata)
  facets <- selection_facets()

  expect_true(is.na(normalized$participants$age))
  expect_true(is.na(normalized$devices$manufacturer))
  expect_equal(
    glcdp:::glc_explorer_filter_participant_ids(
      normalized$participants,
      normalized$participant_characteristics,
      facets$participant
    ),
    "P1"
  )
  expect_false(
    glcdp:::glc_explorer_participant_facets_active(facets$participant)
  )
})

test_that("facet choices use OR within facets and AND across facets", {
  selection <- selection_data()
  facets <- selection_facets()
  facets$participant$age <- c(29, 40)
  facets$participant$sex <- "female"

  expect_setequal(
    glcdp:::glc_explorer_filter_participant_ids(
      selection$participants,
      selection$participant_characteristics,
      facets$participant
    ),
    c("P1", "P3")
  )

  facets$participant$characteristic_name <- "Chronotype"
  facets$participant$characteristic_values <- "evening"
  expect_equal(
    glcdp:::glc_explorer_filter_participant_ids(
      selection$participants,
      selection$participant_characteristics,
      facets$participant
    ),
    "P3"
  )

  facets$device$manufacturer <- "Acme"
  facets$device$model <- c("One", "Two")
  expect_setequal(
    glcdp:::glc_explorer_filter_device_ids(
      selection$devices,
      facets$device
    ),
    c("D1", "D2")
  )
})

test_that("participant ages use an inclusive range with an inactive full span", {
  selection <- selection_data()
  spec <- glcdp:::glc_explorer_age_slider_spec(selection$participants$age)

  expect_equal(spec$min, 29)
  expect_equal(spec$max, 40)
  expect_equal(spec$value, c(29, 40))
  expect_equal(
    glcdp:::glc_explorer_age_filter_value(spec$value, spec$value),
    numeric()
  )
  expect_equal(
    glcdp:::glc_explorer_age_filter_value(c(30, 40), spec$value),
    c(30, 40)
  )

  facets <- selection_facets()
  facets$participant$age <- c(30, 40)
  expect_equal(
    glcdp:::glc_explorer_filter_participant_ids(
      selection$participants,
      selection$participant_characteristics,
      facets$participant
    ),
    "P3"
  )

  single_age <- glcdp:::glc_explorer_age_slider_spec(c("29", NA_character_))
  expect_equal(single_age$value, c(29, 30))
  expect_null(glcdp:::glc_explorer_age_slider_spec(NA_character_))
})

test_that("numeric participant characteristics use an inclusive range", {
  selection <- selection_data()
  characteristics <- selection$participant_characteristics
  spec <- glcdp:::glc_explorer_characteristic_filter_spec(
    characteristics,
    "MEQ"
  )

  expect_equal(spec$type, "numeric")
  expect_equal(spec$name, "MEQ")
  expect_equal(spec$slider$min, 35)
  expect_equal(spec$slider$max, 60)
  expect_equal(spec$slider$step, 1)
  expect_equal(
    glcdp:::glc_explorer_characteristic_filter_value(
      spec$slider$value,
      spec
    ),
    numeric()
  )
  expect_equal(
    glcdp:::glc_explorer_characteristic_filter_value(c(40, 55), spec),
    c(40, 55)
  )

  facets <- selection_facets()
  facets$participant$characteristic_name <- "MEQ"
  facets$participant$characteristic_values <- c(40, 55)
  expect_equal(
    glcdp:::glc_explorer_filter_participant_ids(
      selection$participants,
      characteristics,
      facets$participant
    ),
    c("P2", "P3")
  )

  categorical <- glcdp:::glc_explorer_characteristic_filter_spec(
    characteristics,
    "Chronotype"
  )
  expect_equal(categorical$type, "categorical")
  expect_null(categorical$slider)
  expect_setequal(categorical$values, c("morning", "evening"))
})

test_that("facets and optional IDs resolve associated datasets and groups", {
  selection <- selection_data()
  facets <- selection_facets()
  facets$participant$age <- "40"
  facets$device$model <- "One"

  scope <- glcdp:::glc_explorer_selection_scope(
    selection,
    facets
  )
  expect_equal(scope$participant_ids, "P3")
  expect_equal(scope$device_ids, "D1")
  expect_equal(scope$dataset_ids, "DS3")

  plan <- glcdp:::glc_explorer_build_selection_plan(
    selection_package(),
    selection,
    facets,
    dataset_ids = c("DS2", "DS3"),
    variables = c("timestamp", "lux")
  )
  expect_equal(plan$datasets, "DS3")
  expect_equal(plan$file_groups, "DS3:1")
  expect_equal(plan$participants, "P3")
  expect_equal(plan$devices, "D1")
  expect_true(plan$preview_ready)

  narrowed <- glcdp:::glc_explorer_selection_scope(
    selection,
    selection_facets(),
    participant_ids = "P1",
    device_ids = "D1"
  )
  expect_equal(narrowed$dataset_ids, "DS1")
})

test_that("file groups can narrow datasets and downstream inventory", {
  selection <- selection_data()
  plan <- glcdp:::glc_explorer_build_selection_plan(
    selection_package(),
    selection,
    selection_facets(),
    dataset_ids = c("DS1", "DS2"),
    file_group_ids = "DS1:1",
    variables = "lux"
  )

  expect_equal(plan$requested$dataset_ids, c("DS1", "DS2"))
  expect_equal(plan$requested$file_group_ids, "DS1:1")
  expect_equal(plan$datasets, "DS1")
  expect_equal(plan$file_groups, "DS1:1")
  expect_equal(plan$participants, "P1")
  expect_equal(plan$devices, "D1")
  expect_equal(plan$files$declared_path, "data/DS1.csv")
  expect_true(plan$script_ready)

  scope <- glcdp:::glc_explorer_selection_scope(
    selection,
    selection_facets()
  )
  variables <- glcdp:::glc_explorer_available_variables(
    selection,
    scope,
    c("DS1", "DS2"),
    "DS1:1"
  )
  expect_equal(unique(variables$dataset_id), "DS1")

  choices <- glcdp:::glc_explorer_file_group_choices(selection$groups[1L, ])
  expect_equal(unname(choices), "DS1:1")
  expect_match(names(choices), "D1", fixed = TRUE)
  expect_match(names(choices), "raw", fixed = TRUE)
})

test_that("file-group fields discover repeated participant groups", {
  selection <- selection_data()
  groups <- selection$groups
  groups$device_location[[2L]] <- "hip"
  groups$device_location_type[[2L]] <- "clothing_worn"
  groups$modalities[[2L]] <- "accelerometry"
  groups$role[[2L]] <- "supporting"
  groups$data_state[[3L]] <- "processed"
  groups$variables[[2L]] <- groups$variables[[2L]][1L, , drop = FALSE]

  filters <- glcdp:::glc_explorer_selection_group_filter_spec(
    device_ids = c("D1", "D2"),
    device_locations = "non-dominant wrist",
    roles = "primary",
    variable_names = c("timestamp", "lux"),
    variable_terms = c("datetime", "photopic_illuminance")
  )
  result <- glcdp:::glc_explorer_selection_group_filter_result(
    groups,
    filters
  )

  expect_true(result$active)
  expect_equal(result$candidate_count, 3L)
  expect_equal(result$included_count, 2L)
  expect_equal(result$excluded_count, 1L)
  expect_equal(result$dataset_count, 2L)
  expect_equal(result$groups$file_group_id, c("DS1:1", "DS3:1"))

  processed <- glcdp:::glc_explorer_selection_group_filter_result(
    groups,
    glcdp:::glc_explorer_selection_group_filter_spec(
      data_states = "processed"
    )
  )
  expect_equal(processed$groups$file_group_id, "DS3:1")

  choices <- glcdp:::glc_explorer_selection_group_field_choices(groups)
  expect_setequal(unname(choices$device_ids), c("D1", "D2"))
  expect_setequal(
    unname(choices$device_locations),
    c("hip", "non-dominant wrist")
  )
  expect_setequal(
    unname(choices$modalities),
    c("accelerometry", "light")
  )
  expect_setequal(
    unname(choices$variable_names),
    c("timestamp", "lux")
  )
  expect_setequal(
    unname(choices$variable_terms),
    c("datetime", "photopic_illuminance")
  )
})

test_that("selection plans apply file-group fields before exact IDs", {
  selection <- selection_data()
  selection$groups$device_location[[2L]] <- "hip"
  filters <- glcdp:::glc_explorer_selection_group_filter_spec(
    device_ids = "D1",
    device_locations = "non-dominant wrist",
    modalities = "light"
  )
  plan <- glcdp:::glc_explorer_build_selection_plan(
    selection_package(),
    selection,
    selection_facets(),
    dataset_ids = c("DS1", "DS2", "DS3"),
    group_filters = filters,
    variables = "lux"
  )
  html <- as.character(glcdp:::glc_explorer_selection_issues_tag(plan))

  expect_true(plan$script_ready)
  expect_equal(plan$datasets, c("DS1", "DS3"))
  expect_equal(plan$file_groups, c("DS1:1", "DS3:1"))
  expect_true(plan$group_discovery$active)
  expect_equal(plan$group_discovery$candidate_count, 3L)
  expect_equal(plan$group_discovery$included_count, 2L)
  expect_equal(plan$group_discovery$excluded_count, 1L)
  expect_equal(plan$group_discovery$dataset_count, 2L)
  expect_match(
    html,
    "File-group fields match 2 of 3 eligible groups",
    fixed = TRUE
  )

  scope <- glcdp:::glc_explorer_selection_scope(
    selection,
    selection_facets()
  )
  variables <- glcdp:::glc_explorer_available_variables(
    selection,
    scope,
    c("DS1", "DS2", "DS3"),
    file_group_ids = "DS3:1",
    group_filters = filters
  )
  expect_equal(unique(variables$dataset_id), "DS3")
})

test_that("compatibility detects every blocking group difference", {
  groups <- dplyr::bind_rows(
    selection_group("DS1", "DS1:1", "D1"),
    selection_group("DS2", "DS2:1", "D2")
  )
  compatible <- glcdp:::glc_explorer_selection_compatibility(
    groups,
    c("timestamp", "lux")
  )
  expect_true(compatible$ok)

  timezone <- groups
  timezone$timezone[[2L]] <- "UTC"
  expect_match(
    paste(
      glcdp:::glc_explorer_selection_compatibility(
        timezone,
        c("timestamp", "lux")
      )$issues
    ),
    "time zones",
    fixed = TRUE
  )

  types <- groups
  types$variables[[2L]]$type[[2L]] <- "integer"
  expect_match(
    paste(
      glcdp:::glc_explorer_selection_compatibility(
        types,
        c("timestamp", "lux")
      )$issues
    ),
    "source variable types",
    fixed = TRUE
  )

  factor_levels <- groups
  factor_levels$variables[[1L]]$factor_values <- list(
    character(),
    c("low", "high")
  )
  factor_levels$variables[[1L]]$factor_labels <- list(
    character(),
    c("Low", "High")
  )
  factor_levels$variables[[2L]]$factor_values <- list(
    character(),
    c("high", "low")
  )
  factor_levels$variables[[2L]]$factor_labels <- list(
    character(),
    c("High", "Low")
  )
  expect_match(
    paste(
      glcdp:::glc_explorer_selection_compatibility(
        factor_levels,
        c("timestamp", "lux")
      )$issues
    ),
    "factor levels",
    fixed = TRUE
  )

  columns <- groups
  columns$variables[[2L]] <- columns$variables[[2L]][1L, , drop = FALSE]
  expect_match(
    paste(
      glcdp:::glc_explorer_selection_compatibility(
        columns,
        c("timestamp", "lux")
      )$issues
    ),
    "source columns",
    fixed = TRUE
  )

  unsupported <- groups
  unsupported$format[[2L]] <- "edf"
  expect_match(
    paste(
      glcdp:::glc_explorer_selection_compatibility(
        unsupported,
        c("timestamp", "lux")
      )$issues
    ),
    "unsupported format",
    fixed = TRUE
  )

  modalities <- groups
  modalities$modalities[[2L]] <- c("light", "accelerometry")
  expect_match(
    paste(
      glcdp:::glc_explorer_selection_compatibility(
        modalities,
        c("timestamp", "lux")
      )$issues
    ),
    "modalities",
    fixed = TRUE
  )

  roles <- groups
  roles$role[[2L]] <- "supporting"
  expect_match(
    paste(
      glcdp:::glc_explorer_selection_compatibility(
        roles,
        c("timestamp", "lux")
      )$issues
    ),
    "file roles",
    fixed = TRUE
  )

  states <- groups
  states$data_state[[2L]] <- "processed"
  expect_match(
    paste(
      glcdp:::glc_explorer_selection_compatibility(
        states,
        c("timestamp", "lux")
      )$issues
    ),
    "data states",
    fixed = TRUE
  )

  datetime <- groups
  datetime$datetime_format[[2L]] <- "DD/MM/YYYY HH:mm:ss"
  expect_match(
    paste(
      glcdp:::glc_explorer_selection_compatibility(
        datetime,
        c("timestamp", "lux")
      )$issues
    ),
    "datetime specifications",
    fixed = TRUE
  )

  multiple_devices <- groups
  multiple_devices$dataset_id[[2L]] <- "DS1"
  expect_match(
    paste(
      glcdp:::glc_explorer_selection_compatibility(
        multiple_devices,
        c("timestamp", "lux")
      )$issues
    ),
    "one dataset to multiple devices",
    fixed = TRUE
  )
})

test_that("compatibility ignores differing collection datetime values", {
  groups <- dplyr::bind_rows(
    selection_group("DS1", "DS1:1", "D1"),
    selection_group("DS2", "DS2:1", "D2")
  )
  groups$datetime_source <- "collection"
  groups$datetime_date <- c(
    "2026-01-01 09:00:00",
    "2026-01-02 10:30:00"
  )

  compatibility <- glcdp:::glc_explorer_selection_compatibility(
    groups,
    c("timestamp", "lux")
  )
  expect_true(compatibility$ok)
  expect_false(any(grepl(
    "datetime specifications",
    compatibility$issues,
    fixed = TRUE
  )))
})

test_that("variable filters automatically retain a compatible group subset", {
  skip_if_not_installed("shiny")
  selection <- selection_data()
  selection$groups$variables[[2L]] <-
    selection$groups$variables[[2L]][1L, , drop = FALSE]
  plan <- glcdp:::glc_explorer_build_selection_plan(
    selection_package(),
    selection,
    selection_facets(),
    dataset_ids = c("DS1", "DS2"),
    variables = c("timestamp", "lux")
  )
  html <- as.character(glcdp:::glc_explorer_selection_issues_tag(plan))

  expect_true(plan$script_ready)
  expect_equal(plan$file_groups, "DS1:1")
  expect_equal(plan$group_filter$candidate_count, 2L)
  expect_equal(plan$group_filter$included_count, 1L)
  expect_equal(plan$group_filter$excluded_count, 1L)
  expect_match(html, "include 1 of 2 eligible file groups", fixed = TRUE)
  expect_match(html, "1 is excluded automatically", fixed = TRUE)
  expect_false(grepl("does not provide", html, fixed = TRUE))
  expect_false(grepl("Selection needs attention", html, fixed = TRUE))
})

test_that("semantic terms filter variables and compatible file groups", {
  selection <- selection_data()
  plan <- glcdp:::glc_explorer_build_selection_plan(
    selection_package(),
    selection,
    selection_facets(),
    dataset_ids = c("DS1", "DS2"),
    terms = "photopic_illuminance"
  )

  expect_true(plan$script_ready)
  expect_true(plan$term_filter_active)
  expect_false(plan$name_filter_active)
  expect_null(plan$variable_filter)
  expect_equal(plan$term_filter, "photopic_illuminance")
  expect_equal(plan$variables, "lux")
  expect_equal(plan$terms, "photopic_illuminance")
  expect_equal(plan$file_groups, c("DS1:1", "DS2:1"))

  script <- glcdp:::glc_explorer_selection_script(plan)
  expect_match(
    script,
    'source_terms <- "photopic_illuminance"',
    fixed = TRUE
  )
  expect_match(script, "terms = source_terms", fixed = TRUE)
})

test_that("terms discover groups independently of selected read variables", {
  selection <- selection_data()
  plan <- glcdp:::glc_explorer_build_selection_plan(
    selection_package(),
    selection,
    selection_facets(),
    dataset_ids = c("DS1", "DS2"),
    variables = "timestamp",
    terms = "photopic_illuminance"
  )

  expect_true(plan$script_ready)
  expect_equal(plan$file_groups, c("DS1:1", "DS2:1"))
  expect_equal(plan$variables, "timestamp")
  expect_equal(plan$terms, "photopic_illuminance")
  expect_equal(plan$variable_filter, "timestamp")
  expect_null(plan$term_filter)
  expect_true(plan$name_filter_active)
  expect_true(plan$term_filter_active)

  script <- glcdp:::glc_explorer_selection_script(plan)
  expect_match(script, 'source_variables <- "timestamp"', fixed = TRUE)
  expect_match(script, "source_terms <- NULL", fixed = TRUE)
})

test_that("selection narrowing maps to canonical planner restrictions", {
  selection <- selection_data()
  package <- selection_package()
  requested_datasets <- c("DS2", "DS1")

  dataset_only <- glcdp:::glc_explorer_build_selection_plan(
    package,
    selection,
    selection_facets(),
    dataset_ids = requested_datasets,
    terms = "photopic_illuminance"
  )
  expect_identical(
    dataset_only$planner_restrictions$dataset_id,
    c("DS1", "DS2")
  )
  expect_identical(
    dataset_only$planner_restrictions$file_group,
    character()
  )
  expect_identical(
    dataset_only$planner_restrictions$file_group_basis,
    "omitted"
  )

  explicit_groups <- glcdp:::glc_explorer_build_selection_plan(
    package,
    selection,
    selection_facets(),
    dataset_ids = requested_datasets,
    file_group_ids = c("DS2:1", "DS1:1"),
    terms = "photopic_illuminance"
  )
  expect_identical(
    explicit_groups$planner_restrictions$file_group,
    c("DS1:1", "DS2:1")
  )
  expect_identical(
    explicit_groups$planner_restrictions$file_group_basis,
    "explicit_file_group"
  )

  participant <- glcdp:::glc_explorer_build_selection_plan(
    package,
    selection,
    selection_facets(),
    participant_ids = "P1",
    dataset_ids = requested_datasets,
    variables = "timestamp",
    terms = "photopic_illuminance"
  )
  device <- glcdp:::glc_explorer_build_selection_plan(
    package,
    selection,
    selection_facets(),
    device_ids = "D1",
    dataset_ids = requested_datasets,
    variables = "timestamp",
    terms = "photopic_illuminance"
  )
  field_selection <- selection
  field_selection$groups$role[field_selection$groups$dataset_id == "DS2"] <-
    "supporting"
  group_field <- glcdp:::glc_explorer_build_selection_plan(
    package,
    field_selection,
    selection_facets(),
    dataset_ids = requested_datasets,
    group_filters = list(roles = "primary"),
    variables = "timestamp",
    terms = "photopic_illuminance"
  )

  for (plan in list(participant, device, group_field)) {
    expect_identical(plan$planner_restrictions$dataset_id, c("DS1", "DS2"))
    expect_identical(plan$planner_restrictions$file_group, "DS1:1")
    expect_identical(
      plan$planner_restrictions$file_group_basis,
      "translated_candidate_universe"
    )
    expect_identical(plan$read_restrictions$dataset_id, "DS1")
    expect_identical(plan$read_restrictions$file_group, "DS1:1")
    expect_identical(plan$file_groups, "DS1:1")
  }

  reordered_selection <- field_selection
  reordered_selection$groups <- reordered_selection$groups[
    rev(seq_len(nrow(reordered_selection$groups))),
    ,
    drop = FALSE
  ]
  reordered <- glcdp:::glc_explorer_build_selection_plan(
    package,
    reordered_selection,
    selection_facets(),
    dataset_ids = rev(requested_datasets),
    group_filters = list(roles = "primary"),
    variables = "timestamp",
    terms = "photopic_illuminance"
  )
  expect_identical(
    reordered$planner_restrictions,
    group_field$planner_restrictions
  )
  expect_identical(reordered$file_groups, group_field$file_groups)

  script <- glcdp:::glc_explorer_selection_script(group_field)
  expect_match(script, 'file_groups <- "DS1:1"', fixed = TRUE)
  expect_match(script, 'source_variables <- "timestamp"', fixed = TRUE)
  expect_match(script, "source_terms <- NULL", fixed = TRUE)
})

test_that("automatic compatibility keeps at most one device per dataset", {
  groups <- dplyr::bind_rows(
    selection_group("DS1", "DS1:1", "D1"),
    selection_group("DS1", "DS1:2", "D2"),
    selection_group("DS2", "DS2:1", "D3")
  )

  filtered <- glcdp:::glc_explorer_filter_compatible_groups(
    groups,
    variable_names = "lux"
  )

  expect_true(filtered$active)
  expect_equal(filtered$candidate_count, 3L)
  expect_equal(filtered$included_count, 2L)
  expect_equal(filtered$excluded_count, 1L)
  expect_equal(filtered$groups$file_group_id, c("DS1:1", "DS2:1"))
})

test_that("selection paths and generated R scripts are deterministic and safe", {
  selection <- selection_data()
  facets <- selection_facets()
  plan <- glcdp:::glc_explorer_build_selection_plan(
    selection_package(),
    selection,
    facets,
    dataset_ids = c("DS1", "DS2"),
    variables = c("timestamp", "lux"),
    standardize = "lightlogr"
  )
  reordered <- glcdp:::glc_explorer_build_selection_plan(
    selection_package(),
    selection,
    facets,
    dataset_ids = c("DS2", "DS1"),
    variables = "lux",
    standardize = "none"
  )

  expect_true(plan$script_ready)
  expect_equal(plan$selection_hash, reordered$selection_hash)
  expect_equal(plan$data_directory, reordered$data_directory)
  expect_true(startsWith(plan$data_directory, "data/"))
  expect_equal(plan$estimated_bytes, 300)

  literal_value <- c("a\"b", "line\nnext", "back\\slash")
  literal <- glcdp:::glc_explorer_r_literal(literal_value)
  expect_equal(eval(parse(text = literal)), literal_value)

  script <- glcdp:::glc_explorer_selection_script(plan)
  expect_no_error(parse(text = script))
  expect_match(script, "Registry timestamp", fixed = TRUE)
  expect_match(script, plan$commit, fixed = TRUE)
  expect_match(script, "selection_facets <-", fixed = TRUE)
  expect_match(script, "participant = list", fixed = TRUE)
  expect_match(script, "age = character(0)", fixed = TRUE)
  expect_match(script, "device = list", fixed = TRUE)
  expect_match(script, "glcdp::glc_download", fixed = TRUE)
  expect_match(script, "include = \"data\"", fixed = TRUE)
  expect_match(script, "overwrite = FALSE", fixed = TRUE)
  expect_match(script, "glcdp::glc_read", fixed = TRUE)
  expect_match(script, "source_terms <- NULL", fixed = TRUE)
  expect_match(script, "terms = source_terms", fixed = TRUE)
  expect_match(script, "glc_data <- glcdp::glc_collect", fixed = TRUE)
  expect_match(
    script,
    "# ---- 1. Reproducible selection settings ----",
    fixed = TRUE
  )
  expect_match(
    script,
    "# ---- 2. Download the selected files when needed ----",
    fixed = TRUE
  )
  expect_match(
    script,
    "# ---- 3. Open the local data package ----",
    fixed = TRUE
  )
  expect_match(
    script,
    "# ---- 4. Define the requested data read ----",
    fixed = TRUE
  )
  expect_match(
    script,
    "# ---- 5. Import and combine the data ----",
    fixed = TRUE
  )
  expect_match(
    script,
    "# ---- 6. Clean up temporary handoff objects ----",
    fixed = TRUE
  )
  expect_match(script, '"glc_selection"', fixed = TRUE)
  expect_match(
    script,
    "ls(envir = environment(), all.names = TRUE)",
    fixed = TRUE
  )
  expect_match(script, "glc_data is the final imported table", fixed = TRUE)
  expect_false(grepl("install.packages", script, fixed = TRUE))
  expect_false(grepl("GITHUB_TOKEN", script, fixed = TRUE))
  expect_false(grepl("/private/", script, fixed = TRUE))
})

test_that("handoff cleanup preserves only requested result objects", {
  cleanup <- glcdp:::glc_explorer_cleanup_lines(
    c("repository", "remote_package", "glc_selection"),
    section = 6L
  )
  cleanup_environment <- list2env(
    list(
      repository = "owner/repository",
      glc_selection = "temporary",
      local_package = "package",
      glc_data = "data"
    ),
    parent = baseenv()
  )

  expect_no_error(
    eval(parse(text = paste(cleanup, collapse = "\n")), cleanup_environment)
  )
  expect_setequal(
    ls(cleanup_environment, all.names = TRUE),
    c("local_package", "glc_data")
  )
})

test_that("metadata-only handoffs export package metadata without data reads", {
  package <- metadata_selection_package()
  plan <- glcdp:::glc_explorer_build_selection_plan(
    package,
    selection = NULL,
    facets = list(participant = list(), device = list()),
    mode = "metadata",
    metadata_resources = c("study", "datasets")
  )

  expect_identical(plan$mode, "metadata")
  expect_setequal(plan$metadata_resources, c("study", "datasets"))
  expect_equal(plan$datasets, character())
  expect_equal(plan$file_groups, character())
  expect_false(plan$preview_ready)
  expect_true(plan$script_ready)
  expect_true(startsWith(plan$data_directory, "metadata/"))

  script <- glcdp:::glc_explorer_selection_script(plan)
  expect_no_error(parse(text = script))
  expect_match(script, 'include = "metadata"', fixed = TRUE)
  expect_match(script, "resources = metadata_resources", fixed = TRUE)
  expect_match(script, "local_package <-", fixed = TRUE)
  expect_match(script, "glc_metadata <-", fixed = TRUE)
  expect_match(
    script,
    "# ---- 4. Clean up temporary handoff objects ----",
    fixed = TRUE
  )
  expect_match(script, '"metadata_resources"', fixed = TRUE)
  expect_false(grepl("glc_package", script, fixed = TRUE))
  expect_false(grepl("glc_read\\(", script))
  expect_false(grepl("glc_collect\\(", script))
  expect_equal(
    glcdp:::glc_explorer_download_filename(plan),
    "example-data-metadata.R"
  )

  html <- as.character(
    glcdp:::glc_explorer_download_complete_modal(
      "example-data-metadata.R",
      mode = "metadata"
    )
  )
  expect_match(html, "package metadata", ignore.case = TRUE)
  expect_false(grepl("collect", html, ignore.case = TRUE))
})

test_that("metadata resources and final row limits are validated", {
  package <- metadata_selection_package()

  expect_setequal(
    glcdp:::glc_explorer_metadata_resources(package),
    c("study", "datasets", "devices")
  )
  expect_error(
    glcdp:::glc_explorer_metadata_resources(package, "unknown"),
    "Unknown metadata resource",
    fixed = TRUE
  )
  expect_equal(glcdp:::glc_explorer_row_limit(), Inf)
  expect_equal(glcdp:::glc_explorer_row_limit(25), 25)
  expect_error(glcdp:::glc_explorer_row_limit(0), "positive whole number")
  expect_error(glcdp:::glc_explorer_row_limit(-1), "positive whole number")
  expect_error(glcdp:::glc_explorer_row_limit(1.5), "positive whole number")

  plan <- glcdp:::glc_explorer_build_selection_plan(
    selection_package(),
    selection_data(),
    selection_facets(),
    dataset_ids = "DS1",
    n_max = 250
  )
  expect_equal(plan$n_max, 250)
  script <- glcdp:::glc_explorer_selection_script(plan)
  expect_match(script, "row_limit <- 250", fixed = TRUE)
  expect_match(script, "n_max = row_limit", fixed = TRUE)

  empty <- glcdp:::glc_explorer_build_selection_plan(
    selection_package(),
    selection_data(),
    selection_facets(),
    mode = "data",
    dataset_ids = character()
  )
  expect_false(empty$script_ready)
  expect_match(
    empty$issues,
    "Select at least one eligible dataset",
    fixed = TRUE
  )
})

test_that("an empty variable selection imports every available variable", {
  plan <- glcdp:::glc_explorer_build_selection_plan(
    selection_package(),
    selection_data(),
    selection_facets(),
    dataset_ids = "DS1",
    variables = character()
  )

  expect_true(plan$script_ready)
  expect_false(plan$variable_filter_active)
  expect_false(plan$term_filter_active)
  expect_null(plan$variable_filter)
  expect_null(plan$term_filter)
  expect_setequal(plan$variables, c("timestamp", "lux"))
  expect_equal(plan$requested$variables, character())

  summary <- glcdp:::glc_explorer_selection_summary_table(plan)
  expect_equal(
    summary$Value[summary$Selection == "Source variables"],
    "All available (2)"
  )
  script <- glcdp:::glc_explorer_selection_script(plan)
  expect_match(script, "source_variables <- NULL", fixed = TRUE)
  expect_match(script, "all declared source variables", fixed = TRUE)
})

test_that("selection summaries abbreviate long identifier lists", {
  values <- sprintf("MELIDOS_IZTECH_S%03d", seq_len(17L))
  display <- glcdp:::glc_explorer_display_selection_values(values)

  expect_match(display, "MELIDOS_IZTECH_S001", fixed = TRUE)
  expect_match(display, "MELIDOS_IZTECH_S008", fixed = TRUE)
  expect_match(display, "\u2026 (+9 more)", fixed = TRUE)
  expect_false(grepl("MELIDOS_IZTECH_S009", display, fixed = TRUE))
  expect_equal(
    glcdp:::glc_explorer_display_selection_values(character()),
    "\u2014"
  )
})

test_that("preview is gated and uses a configurable row limit", {
  invalid <- list(
    preview_ready = FALSE,
    issues = "Select at least one eligible dataset."
  )
  expect_error(
    glcdp:::glc_explorer_preview_selection(NULL, invalid),
    class = "glcdp_explorer_invalid_selection"
  )

  package <- glc_open(make_multi_dataset_fixture(), quiet = TRUE)
  selection <- glcdp:::glc_explorer_load_selection(package)
  expect_equal(
    unique(selection$groups$device_location),
    "non-dominant wrist"
  )
  expect_equal(unique(selection$groups$device_location_type), "body_worn")
  expect_equal(unique(selection$groups$temporal_type), "fixed_interval")
  expect_equal(unique(selection$groups$temporal_value), 60)
  expect_true(all(
    c(
      "label",
      "description",
      "unit",
      "factor_values",
      "factor_labels",
      "factor_descriptions"
    ) %in%
      names(selection$groups$variables[[1L]])
  ))
  plan <- glcdp:::glc_explorer_build_selection_plan(
    package,
    selection,
    selection_facets(),
    dataset_ids = c("DS1", "DS2"),
    variables = "lux"
  )
  expect_true(plan$preview_ready)
  expect_equal(length(plan$preview_files), 2L)
  preview <- glcdp:::glc_explorer_preview_selection(package, plan)
  expect_equal(nrow(preview), 4L)
  one_file_preview <- glcdp:::glc_explorer_preview_selection(
    package,
    plan,
    file_limit = 1L
  )
  expect_equal(nrow(one_file_preview), 2L)
  expect_equal(
    glcdp:::glc_explorer_preview_files(plan, 1L),
    plan$preview_files[[1L]]
  )
  transfer <- glcdp:::glc_explorer_preview_transfer(plan, 1L)
  expect_equal(length(transfer$files), 1L)
  expect_equal(transfer$available, 2L)

  all_variables <- glcdp:::glc_explorer_build_selection_plan(
    package,
    selection,
    selection_facets(),
    dataset_ids = "DS1",
    variables = character()
  )
  all_preview <- glcdp:::glc_explorer_preview_selection(
    package,
    all_variables
  )
  expect_true(all(
    c("timestamp", "lux", "worn", "quality") %in% names(all_preview)
  ))

  expect_equal(glcdp:::glc_explorer_preview_row_limit(NULL), 10L)
  expect_equal(glcdp:::glc_explorer_preview_row_limit(25), 25L)
  expect_equal(glcdp:::glc_explorer_preview_row_limit(0), 1L)
  expect_equal(glcdp:::glc_explorer_preview_row_limit(2000), 1000L)
  expect_equal(glcdp:::glc_explorer_preview_row_limit("invalid"), 10L)
  expect_equal(glcdp:::glc_explorer_preview_file_limit(NULL, 5L), 2L)
  expect_equal(glcdp:::glc_explorer_preview_file_limit(10L, 5L), 5L)
  expect_equal(glcdp:::glc_explorer_preview_file_limit(0L, 5L), 1L)
  expect_equal(glcdp:::glc_explorer_preview_file_limit("invalid", 5L), 2L)
  expect_equal(glcdp:::glc_explorer_preview_file_limit(2L, 0L), 0L)
})

test_that("preview imports the stable schema 3 column contract", {
  package <- glc_open(make_v3_contract_fixture(), quiet = TRUE)
  selection <- glcdp:::glc_explorer_load_selection(package)
  plan <- glcdp:::glc_explorer_build_selection_plan(
    package,
    selection,
    selection_facets(),
    dataset_ids = "DS1"
  )

  expect_true(plan$preview_ready)
  preview <- glcdp:::glc_explorer_preview_selection(package, plan)
  expect_equal(nrow(preview), 2L)
  expect_s3_class(preview$Id, "factor")
  expect_s3_class(preview$Datetime, "POSIXct")
  expect_equal(preview$worn, c(TRUE, FALSE))
  expect_equal(levels(preview$quality), c("Good", "Bad"))
  expect_equal(preview$file.name, c("raw-a.csv", NA_character_))
})

test_that("preview display formats parsed datetimes without changing data", {
  datetime <- as.POSIXct(
    c("2026-01-01 08:00:00", NA_character_),
    tz = "Europe/Berlin"
  )
  preview <- tibble::tibble(
    Datetime = datetime,
    source_datetime = c("raw timestamp", NA_character_),
    value = c(1, 2)
  )

  display <- glcdp:::glc_explorer_format_preview(preview)
  expect_type(display$Datetime, "character")
  expect_match(display$Datetime[[1L]], "^2026-01-01 08:00:00")
  expect_true(is.na(display$Datetime[[2L]]))
  expect_identical(display$source_datetime, preview$source_datetime)
  expect_identical(display$value, preview$value)
  expect_s3_class(preview$Datetime, "POSIXct")
})

test_that("download completion modal explains the next steps", {
  html <- as.character(
    glcdp:::glc_explorer_download_complete_modal("demo-selection.R")
  )
  expect_match(html, "R script downloaded", fixed = TRUE)
  expect_match(html, "demo-selection.R", fixed = TRUE)
  expect_match(html, "download, import, and collect", fixed = TRUE)
  expect_match(html, "different data package", fixed = TRUE)
  expect_match(html, "close this browser tab or app", fixed = TRUE)
})

test_that("selection UI exposes the complete handoff workflow", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  html <- as.character(glcdp:::selection_handoff_ui("handoff"))
  expect_match(html, "1. Package &amp; metadata", fixed = TRUE)
  expect_match(html, "Package and metadata only", fixed = TRUE)
  expect_match(html, "Import matching data", fixed = TRUE)
  expect_match(html, "handoff-metadata_resources", fixed = TRUE)
  expect_match(html, "2. File groups", fixed = TRUE)
  expect_match(html, "4. Variables &amp; rows", fixed = TRUE)
  expect_match(
    html,
    "Filter file groups in Package contents",
    fixed = TRUE
  )
  expect_match(html, "Participants", fixed = TRUE)
  expect_match(html, "handoff-participant_age_filter", fixed = TRUE)
  expect_match(html, "handoff-characteristic_value_filter", fixed = TRUE)
  expect_match(html, "Devices", fixed = TRUE)
  expect_match(html, "File groups", fixed = TRUE)
  expect_match(html, "Wearing position", fixed = TRUE)
  expect_match(html, "Contains variables", fixed = TRUE)
  expect_match(html, "Contains semantic terms", fixed = TRUE)
  expect_match(html, "handoff-group_device_location", fixed = TRUE)
  expect_match(html, "handoff-group_variable", fixed = TRUE)
  expect_match(html, "handoff-group_term", fixed = TRUE)
  expect_match(html, "Datasets (required)", fixed = TRUE)
  expect_match(html, "Exact file groups (advanced)", fixed = TRUE)
  expect_match(html, "Every matching file group", fixed = TRUE)
  expect_match(html, "Source variables (optional)", fixed = TRUE)
  expect_match(html, "All source variables", fixed = TRUE)
  expect_match(html, "Semantic terms (optional)", fixed = TRUE)
  expect_match(html, "All semantic terms", fixed = TRUE)
  expect_match(html, "handoff-variable_terms", fixed = TRUE)
  expect_match(html, "Use all variables", fixed = TRUE)
  expect_match(html, ">Primary<", fixed = TRUE)
  expect_match(html, "handoff-variables_primary", fixed = TRUE)
  expect_false(grepl("Recommended", html, fixed = TRUE))
  expect_false(grepl("handoff-variables_select_all", html, fixed = TRUE))
  expect_match(html, "handoff-file_groups_use_all", fixed = TRUE)
  expect_match(html, "handoff-file_group_filters_clear", fixed = TRUE)
  expect_match(html, "LightLogR-compatible", fixed = TRUE)
  expect_match(html, "5. Review", fixed = TRUE)
  expect_match(html, "handoff-review_sections", fixed = TRUE)
  expect_match(html, "handoff-review-accordion", fixed = TRUE)
  expect_match(html, "Selection summary", fixed = TRUE)
  expect_match(html, "Included file groups", fixed = TRUE)
  expect_match(
    html,
    'data-value="selection">',
    fixed = TRUE
  )
  expect_match(
    html,
    'aria-expanded="true"',
    fixed = TRUE
  )
  expect_match(
    html,
    'data-value="groups">',
    fixed = TRUE
  )
  expect_match(
    html,
    'class="accordion-button collapsed"',
    fixed = TRUE
  )
  expect_match(html, "handoff-selection_summary", fixed = TRUE)
  expect_match(html, "handoff-group_summary_count", fixed = TRUE)
  expect_match(html, "handoff-group_summary_pagination", fixed = TRUE)
  expect_match(html, "6. Preview", fixed = TRUE)
  expect_match(html, "Maximum rows per file", fixed = TRUE)
  expect_match(html, "handoff-max_rows_per_file", fixed = TRUE)
  expect_match(html, "Preview rows per file", fixed = TRUE)
  expect_match(html, "handoff-preview_rows", fixed = TRUE)
  expect_match(html, 'value="10"', fixed = TRUE)
  expect_match(html, "Files to preview", fixed = TRUE)
  expect_match(html, "handoff-preview_files", fixed = TRUE)
  expect_match(html, 'id="handoff-preview_files"', fixed = TRUE)
  expect_match(
    html,
    'id="handoff-preview_files"[^>]*value="2"'
  )
  expect_match(html, "7. Export to R", fixed = TRUE)
  expect_match(html, "Continue to Export to R", fixed = TRUE)
  expect_match(html, "handoff-summary_continue_ui", fixed = TRUE)
  expect_match(html, "handoff-preview_continue", fixed = TRUE)
  expect_match(html, 'class="handoff-wizard ', fixed = TRUE)
  expect_false(grepl("handoff-sidebar", html, fixed = TRUE))
  expect_false(grepl("handoff-selection_steps", html, fixed = TRUE))
  wizard_steps <- c(
    "package",
    "groups",
    "people",
    "variables",
    "summary",
    "preview",
    "export"
  )
  step_positions <- vapply(
    wizard_steps,
    function(step)
      regexpr(
        paste0('data-value="', step, '"'),
        html,
        fixed = TRUE
      )[[1L]],
    integer(1)
  )
  expect_true(all(step_positions > 0L))
  expect_true(all(diff(step_positions) > 0L))
  expect_match(html, "handoff-preview_action", fixed = TRUE)
  expect_match(html, "handoff-script_download_ui", fixed = TRUE)
  expect_match(html, "handoff-wizard-layout", fixed = TRUE)
  expect_match(html, "handoff-control-column", fixed = TRUE)
  expect_match(html, "handoff-information-column", fixed = TRUE)
  expect_match(html, "handoff-control-grid", fixed = TRUE)
  expect_match(html, "handoff-step-content", fixed = TRUE)
  expect_match(html, "handoff-step-nav", fixed = TRUE)
  expect_match(html, "handoff-nav-action", fixed = TRUE)
  expect_match(html, "handoff-bounded-selectize", fixed = TRUE)
  expect_match(html, "handoff-groups_back", fixed = TRUE)
  expect_match(html, "handoff-export_data_back", fixed = TRUE)
  expect_false(grepl("Continue from top to bottom", html, fixed = TRUE))
  expect_false(grepl("Shiny.setInputValue", html, fixed = TRUE))

  position <- function(value) {
    regexpr(value, html, fixed = TRUE)[[1L]]
  }
  expect_lt(
    position('id="handoff-selection_summary"'),
    position('id="handoff-summary_back"')
  )
  expect_lt(
    position('id="handoff-group_summary"'),
    position('id="handoff-summary_back"')
  )
  expect_lt(
    position('id="handoff-preview_action"'),
    position('id="handoff-preview_table"')
  )
  expect_lt(
    position('id="handoff-preview_table"'),
    position('id="handoff-preview_data_back"')
  )
  expect_lt(
    position('id="handoff-script_download_ui"'),
    position('id="handoff-script"')
  )
  expect_lt(
    position('id="handoff-script"'),
    position('id="handoff-export_data_back"')
  )
})

test_that("wizard content scrolls without displacing its navigation", {
  skip_if_not_installed("bslib")

  theme <- glcdp:::glc_explorer_theme()
  rules <- theme$layers[[length(theme$layers)]]$rules

  expect_match(
    rules,
    paste0(
      ".handoff-wizard-step {\n",
      "  width: 100%;\n",
      "  height: 100%;\n",
      "  min-height: 0;"
    ),
    fixed = TRUE
  )
  expect_match(
    rules,
    paste0(
      ".handoff-step-content {\n",
      "  flex: 1 1 auto;\n",
      "  min-height: 0;\n",
      "  overflow-y: auto;"
    ),
    fixed = TRUE
  )
  expect_match(
    rules,
    paste0(
      ".handoff-step-nav {\n",
      "  display: grid;\n",
      "  grid-template-columns: repeat(2, minmax(0, 1fr));"
    ),
    fixed = TRUE
  )
  expect_match(rules, "  flex: 0 0 auto;", fixed = TRUE)
})

test_that("recommended variables prefer primary declarations and fall back", {
  variables <- tibble::tibble(
    name = c("timestamp", "lux", "quality"),
    primary = c(FALSE, TRUE, TRUE)
  )
  expect_equal(
    glcdp:::glc_explorer_default_variables(variables),
    c("lux", "quality")
  )
  variables$primary <- FALSE
  expect_equal(
    glcdp:::glc_explorer_default_variables(variables),
    c("timestamp", "lux", "quality")
  )
})

test_that("semantic term choices retain schema values and readable labels", {
  variables <- tibble::tibble(
    term = c("melanopic_edi", "melanopic_edi", "datetime"),
    term_name = c(NA_character_, "Melanopic EDI", "Date and time")
  )

  choices <- glcdp:::glc_explorer_variable_term_choices(variables)

  expect_equal(unname(choices), c("datetime", "melanopic_edi"))
  expect_match(names(choices)[[1L]], "Date and time", fixed = TRUE)
  expect_match(names(choices)[[2L]], "Melanopic EDI", fixed = TRUE)
})

test_that("temporarily unavailable choices retain explicit selections", {
  current <- c("Device one" = "D1")
  universe <- c("Device one" = "D1", "Device two" = "D2")
  choices <- glcdp:::glc_explorer_preserve_selected_choices(
    current,
    selected = "D2",
    universe = universe
  )

  expect_equal(unname(choices), c("D1", "D2"))
  expect_equal(names(choices), c("Device one", "Device two"))
  expect_equal(
    glcdp:::glc_explorer_preserve_selected_choices(
      current,
      selected = "unknown",
      universe = universe
    ),
    current
  )
})

test_that("metadata handoff stays lightweight until data mode is requested", {
  skip_if_not_installed("shiny")
  load_count <- 0L

  shiny::testServer(
    glcdp:::selection_handoff_server,
    args = list(
      package = shiny::reactive(metadata_selection_package()),
      active = shiny::reactive(TRUE),
      default_mode = "metadata",
      load_selection = function(package) {
        load_count <<- load_count + 1L
        selection_data()
      },
      schedule_after_flush = function(callback, session) callback()
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      expect_equal(load_count, 0L)
      expect_identical(state$handoff_mode(), "metadata")
      expect_identical(state$plan()$mode, "metadata")
      expect_true(state$plan()$script_ready)
      expect_null(state$selection())

      session$setInputs(handoff_mode = "data")
      session$flushReact()
      expect_equal(load_count, 1L)
      expect_identical(state$handoff_mode(), "data")
      expect_type(state$selection(), "list")
    }
  )
})

test_that("file-group handoff seeds survive lazy loading and stay exact", {
  skip_if_not_installed("shiny")
  package_value <- selection_package()
  preselection_seed <- shiny::reactiveVal(NULL)
  load_count <- 0L
  pending_callbacks <- list()
  run_next_callback <- function() {
    callback <- pending_callbacks[[1L]]
    pending_callbacks <<- pending_callbacks[-1L]
    shiny::maskReactiveContext(callback())
  }

  shiny::testServer(
    glcdp:::selection_handoff_server,
    args = list(
      package = shiny::reactive(package_value),
      active = shiny::reactive(TRUE),
      preselection = shiny::reactive(preselection_seed()),
      default_mode = "metadata",
      load_selection = function(package) {
        load_count <<- load_count + 1L
        selection_data()
      },
      schedule_after_flush = function(callback, session) {
        pending_callbacks[[length(pending_callbacks) + 1L]] <<- callback
      }
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      expect_equal(load_count, 0L)
      expect_length(pending_callbacks, 0L)
      expect_identical(state$handoff_mode(), "metadata")

      preselection_seed(list(
        package_key = glcdp:::glc_explorer_package_key(package_value),
        request_id = 1L,
        dataset_ids = c("DS1", "DS2"),
        file_group_ids = c("DS1:1", "DS2:1", "missing:1")
      ))
      session$flushReact()
      expect_null(state$applied_preselection())

      session$setInputs(handoff_mode = "data")
      session$flushReact()
      expect_equal(load_count, 0L)
      expect_identical(state$status()$state, "loading")
      expect_length(pending_callbacks, 1L)

      # The first delayed callback loads the selection. Running it without a
      # reactive context catches unisolated reads from asynchronous work.
      expect_no_error(run_next_callback())
      session$flushReact()
      expect_equal(load_count, 1L)
      expect_null(state$applied_preselection())
      expect_length(pending_callbacks, 1L)

      # Selection-dependent observers have now queued their default/empty
      # input updates. Apply the exact seed only in the following flush phase
      # so those updates cannot overwrite it in the browser.
      expect_no_error(run_next_callback())
      session$flushReact()
      expect_length(pending_callbacks, 0L)
      applied <- state$applied_preselection()
      expect_equal(applied$request_id, "1")
      expect_equal(applied$requested_group_count, 3L)
      expect_equal(applied$file_group_ids, c("DS1:1", "DS2:1"))
      expect_equal(applied$dataset_ids, c("DS1", "DS2"))

      session$setInputs(
        dataset_ids = applied$dataset_ids,
        file_group_ids = applied$file_group_ids
      )
      session$flushReact()
      expect_equal(state$plan()$file_groups, c("DS1:1", "DS2:1"))

      session$setInputs(handoff_mode = "metadata")
      session$flushReact()
      expect_identical(state$handoff_mode(), "metadata")
      expect_identical(state$plan()$mode, "metadata")
      expect_identical(state$applied_preselection(), applied)
      expect_equal(load_count, 1L)

      session$setInputs(handoff_mode = "data")
      session$flushReact()
      expect_identical(state$handoff_mode(), "data")
      expect_identical(state$plan()$mode, "data")
      expect_equal(state$plan()$file_groups, c("DS1:1", "DS2:1"))
      expect_identical(state$applied_preselection(), applied)
      expect_equal(load_count, 1L)

      preselection_seed(list(
        package_key = "remote:other/package:deadbeef",
        request_id = 2L,
        dataset_ids = "DS3",
        file_group_ids = "DS3:1"
      ))
      session$flushReact()
      expect_equal(state$applied_preselection()$request_id, "1")

      preselection_seed(list(
        package_key = glcdp:::glc_explorer_package_key(package_value),
        request_id = 1L,
        dataset_ids = "DS3",
        file_group_ids = "DS3:1"
      ))
      session$flushReact()
      expect_equal(
        state$applied_preselection()$file_group_ids,
        c(
          "DS1:1",
          "DS2:1"
        )
      )
    }
  )
})

test_that("selection module gates, previews, and invalidates downstream state", {
  skip_if_not_installed("shiny")
  package <- selection_package()
  active <- shiny::reactiveVal(TRUE)
  load_count <- 0L
  fail_preview <- FALSE
  preview_calls <- 0L
  preview_limits <- integer()
  preview_file_limits <- integer()
  selected_tabs <- character()

  shiny::testServer(
    glcdp:::selection_handoff_server,
    args = list(
      package = shiny::reactive(package),
      active = active,
      load_selection = function(package) {
        load_count <<- load_count + 1L
        selection_data()
      },
      preview_selection = function(package, plan, n_max, file_limit) {
        preview_calls <<- preview_calls + 1L
        preview_limits <<- c(preview_limits, n_max)
        preview_file_limits <<- c(preview_file_limits, file_limit)
        if (fail_preview) {
          stop("preview failed")
        }
        tibble::tibble(
          dataset = plan$datasets,
          value = seq_along(plan$datasets)
        )
      },
      schedule_after_flush = function(callback, session) callback(),
      select_nav = function(id, selected, session) {
        expect_equal(id, "handoff_tab")
        selected_tabs <<- c(selected_tabs, selected)
      }
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      expect_equal(load_count, 1L)
      expect_equal(state$status()$state, "success")
      expect_null(state$preview())

      session$setInputs(
        participant_age = character(),
        participant_sex = character(),
        participant_gender = character(),
        characteristic_name = "",
        characteristic_values = character(),
        participant_ids = character(),
        device_manufacturer = character(),
        device_model = character(),
        device_sensor_type = character(),
        device_ids = character(),
        dataset_ids = c("DS1", "DS2"),
        group_device_id = character(),
        group_device_location = character(),
        group_location_type = character(),
        group_modality = character(),
        group_role = character(),
        group_state = character(),
        group_variable = character(),
        group_term = character(),
        file_group_ids = character(),
        variable_terms = character(),
        variables = c("timestamp", "lux"),
        standardization = "lightlogr"
      )
      session$flushReact()
      expect_true(state$plan()$preview_ready)
      expect_true(state$plan()$script_ready)
      expect_equal(state$plan()$datasets, c("DS1", "DS2"))
      expect_equal(state$group_summary_page()$total, 2L)
      expect_equal(nrow(state$group_summary_page()$data), 2L)

      session$setInputs(
        file_group_ids = "DS1:1",
        group_device_id = "D2"
      )
      session$flushReact()
      expect_equal(state$effective_file_group_ids(), character())
      expect_equal(state$plan()$datasets, character())
      expect_false(state$plan()$script_ready)
      session$setInputs(group_device_id = character())
      session$flushReact()
      expect_equal(state$effective_file_group_ids(), "DS1:1")
      expect_equal(state$plan()$datasets, "DS1")
      expect_true(state$plan()$script_ready)
      session$setInputs(
        file_group_ids = character(),
        group_device_id = character()
      )

      session$setInputs(group_device_id = "D1")
      session$flushReact()
      expect_equal(state$plan()$datasets, "DS1")
      expect_equal(state$group_filter_result()$included_count, 1L)
      expect_equal(state$group_filter_result()$excluded_count, 1L)
      session$setInputs(group_device_id = character())

      session$setInputs(
        participant_age = c(40, 40),
        dataset_ids = c("DS1", "DS2"),
        file_group_ids = c("DS1:1", "DS2:1")
      )
      session$flushReact()
      expect_equal(state$effective_file_group_ids(), character())
      expect_false(state$plan()$script_ready)
      session$setInputs(participant_age = character())
      session$flushReact()
      expect_equal(
        state$effective_file_group_ids(),
        c("DS1:1", "DS2:1")
      )
      expect_equal(state$plan()$datasets, c("DS1", "DS2"))
      expect_true(state$plan()$script_ready)
      session$setInputs(file_group_ids = character())

      session$setInputs(
        characteristic_name = "MEQ",
        characteristic_values = c(40, 55),
        dataset_ids = c("DS1", "DS2", "DS3")
      )
      session$flushReact()
      expect_equal(state$plan()$datasets, c("DS2", "DS3"))
      expect_equal(state$plan()$participants, c("P2", "P3"))

      session$setInputs(
        characteristic_name = "",
        characteristic_values = character(),
        variable_terms = "photopic_illuminance",
        variables = character()
      )
      session$flushReact()
      expect_true(state$plan()$script_ready)
      expect_equal(state$plan()$variables, "lux")
      expect_equal(state$plan()$term_filter, "photopic_illuminance")

      session$setInputs(
        file_group_ids = "DS1:1",
        variable_terms = character(),
        variables = c("timestamp", "lux")
      )
      session$flushReact()
      expect_equal(state$plan()$datasets, "DS1")
      expect_equal(state$plan()$file_groups, "DS1:1")

      session$setInputs(
        variable_terms = character(),
        variables = character()
      )
      session$flushReact()
      expect_true(state$plan()$script_ready)
      expect_null(state$plan()$variable_filter)
      expect_setequal(state$plan()$variables, c("timestamp", "lux"))

      session$setInputs(
        dataset_ids = c("DS1", "DS2"),
        file_group_ids = character(),
        preview_files = 2L
      )
      session$flushReact()
      session$setInputs(build_preview = 1L)
      session$flushReact()
      expect_equal(preview_calls, 1L)
      expect_equal(preview_limits, 10L)
      expect_equal(preview_file_limits, 2L)
      expect_equal(nrow(state$preview()), 2L)
      expect_equal(state$preview_status()$state, "success")

      session$setInputs(dataset_ids = character())
      session$flushReact()
      expect_false(state$plan()$preview_ready)
      expect_null(state$preview())

      session$setInputs(
        participant_age = "40",
        dataset_ids = "DS3",
        file_group_ids = character(),
        variable_terms = character(),
        variables = c("timestamp", "lux")
      )
      session$flushReact()
      expect_equal(state$plan()$datasets, "DS3")
      expect_equal(state$plan()$participants, "P3")

      fail_preview <<- TRUE
      session$setInputs(preview_rows = 25L, preview_files = 25L)
      session$flushReact()
      expect_null(state$preview())
      session$setInputs(build_preview = 2L)
      session$flushReact()
      expect_equal(preview_calls, 2L)
      expect_equal(preview_limits, c(10L, 25L))
      expect_equal(preview_file_limits, c(2L, 1L))
      expect_null(state$preview())
      expect_equal(state$preview_status()$state, "error")
      expect_match(state$preview_status()$message, "preview failed")

      session$setInputs(summary_continue = 1L)
      session$flushReact()
      session$setInputs(preview_continue = 1L)
      session$flushReact()
      expect_equal(selected_tabs, c("preview", "export"))

      session$setInputs(groups_back = 1L)
      session$setInputs(people_back = 1L)
      session$setInputs(variables_back = 1L)
      session$setInputs(summary_back = 1L)
      session$setInputs(preview_data_back = 1L)
      session$setInputs(export_data_back = 1L)
      session$flushReact()
      expect_equal(
        utils::tail(selected_tabs, 6L),
        c("package", "groups", "people", "variables", "summary", "preview")
      )

      active(FALSE)
      active(TRUE)
      session$flushReact()
      expect_equal(load_count, 1L)
    }
  )
})

test_that("selection module reports loading failures", {
  skip_if_not_installed("shiny")

  shiny::testServer(
    glcdp:::selection_handoff_server,
    args = list(
      package = shiny::reactive(selection_package()),
      active = shiny::reactive(TRUE),
      load_selection = function(package) stop("selection failed"),
      schedule_after_flush = function(callback, session) callback()
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      expect_null(state$selection())
      expect_equal(state$status()$state, "error")
      expect_match(state$status()$message, "selection failed")
    }
  )
})

test_that("selection showcase creates a Shiny application", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  package <- glc_open(make_glc_fixture(), quiet = TRUE)

  expect_s3_class(glcdp:::selection_handoff_app(package), "shiny.appobj")
})
