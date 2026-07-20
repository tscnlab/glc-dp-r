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
  variables = tibble::tibble(
    name = c("timestamp", "lux"),
    type = c("string", "numeric"),
    primary = c(FALSE, TRUE)
  )
) {
  tibble::tibble(
    dataset_id = dataset_id,
    file_group = 1L,
    file_group_id = file_group_id,
    device_id = device_id,
    format = "csv",
    timezone = "Europe/Berlin",
    modalities = list("light"),
    role = "primary",
    data_state = "raw",
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
      participant_id = c("P1", "P2", "P3", "P4"),
      characteristic_name = rep("Chronotype", 4L),
      characteristic_value = c("morning", "morning", "evening", "evening")
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
})

test_that("compatibility feedback points to the file-group control", {
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

  expect_false(plan$script_ready)
  expect_match(html, "current file groups cannot be collected", fixed = TRUE)
  expect_match(html, "Data selection", fixed = TRUE)
  expect_match(html, "compatible subset of file groups", fixed = TRUE)
  expect_false(grepl("Selected file groups", html, fixed = TRUE))
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
  expect_match(script, "glc_data is the final imported table", fixed = TRUE)
  expect_false(grepl("install.packages", script, fixed = TRUE))
  expect_false(grepl("GITHUB_TOKEN", script, fixed = TRUE))
  expect_false(grepl("/private/", script, fixed = TRUE))
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
  expect_null(plan$variable_filter)
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
  expect_match(html, "Participants", fixed = TRUE)
  expect_match(html, "handoff-participant_age_filter", fixed = TRUE)
  expect_match(html, "Devices", fixed = TRUE)
  expect_match(html, "Data selection", fixed = TRUE)
  expect_match(html, "Datasets (required)", fixed = TRUE)
  expect_match(html, "File groups (optional)", fixed = TRUE)
  expect_match(html, "All file groups", fixed = TRUE)
  expect_match(html, "Source variables (optional)", fixed = TRUE)
  expect_match(html, "All source variables", fixed = TRUE)
  expect_match(html, "Use all variables", fixed = TRUE)
  expect_match(html, "handoff-file_groups_use_all", fixed = TRUE)
  expect_match(html, "LightLogR-compatible", fixed = TRUE)
  expect_match(html, "Selection summary", fixed = TRUE)
  expect_match(html, "Preview", fixed = TRUE)
  expect_match(html, "Rows to read per file", fixed = TRUE)
  expect_match(html, "handoff-preview_rows", fixed = TRUE)
  expect_match(html, 'value="10"', fixed = TRUE)
  expect_match(html, "Export to R (R script)", fixed = TRUE)
  expect_match(html, "fa-1", fixed = TRUE)
  expect_match(html, "fa-2", fixed = TRUE)
  expect_match(html, "fa-3", fixed = TRUE)
  expect_match(html, "Continue to preview", fixed = TRUE)
  expect_match(html, "Continue to Export to R", fixed = TRUE)
  expect_match(html, "handoff-summary_continue", fixed = TRUE)
  expect_match(html, "handoff-preview_continue", fixed = TRUE)
  expect_match(
    html,
    'data-value="Participants">[\\s\\S]{0,300}accordion-button collapsed',
    perl = TRUE
  )
  expect_match(
    html,
    'data-value="Data selection">[\\s\\S]{0,300}aria-expanded="true"',
    perl = TRUE
  )
  expect_match(html, "handoff-preview_action", fixed = TRUE)
  expect_match(html, "handoff-script_download_ui", fixed = TRUE)
  expect_false(grepl("Shiny.setInputValue", html, fixed = TRUE))
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

test_that("selection module gates, previews, and invalidates downstream state", {
  skip_if_not_installed("shiny")
  package <- selection_package()
  active <- shiny::reactiveVal(TRUE)
  load_count <- 0L
  fail_preview <- FALSE
  preview_calls <- 0L
  preview_limits <- integer()
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
      preview_selection = function(package, plan, n_max) {
        preview_calls <<- preview_calls + 1L
        preview_limits <<- c(preview_limits, n_max)
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
        file_group_ids = character(),
        variables = c("timestamp", "lux"),
        standardization = "lightlogr"
      )
      session$flushReact()
      expect_true(state$plan()$preview_ready)
      expect_true(state$plan()$script_ready)
      expect_equal(state$plan()$datasets, c("DS1", "DS2"))

      session$setInputs(file_group_ids = "DS1:1")
      session$flushReact()
      expect_equal(state$plan()$datasets, "DS1")
      expect_equal(state$plan()$file_groups, "DS1:1")

      session$setInputs(variables = character())
      session$flushReact()
      expect_true(state$plan()$script_ready)
      expect_null(state$plan()$variable_filter)
      expect_setequal(state$plan()$variables, c("timestamp", "lux"))

      session$setInputs(build_preview = 1L)
      session$flushReact()
      expect_equal(preview_calls, 1L)
      expect_equal(preview_limits, 10L)
      expect_equal(nrow(state$preview()), 1L)
      expect_equal(state$preview_status()$state, "success")

      session$setInputs(dataset_ids = character())
      session$flushReact()
      expect_false(state$plan()$preview_ready)
      expect_null(state$preview())

      session$setInputs(
        participant_age = "40",
        dataset_ids = "DS3",
        file_group_ids = character(),
        variables = c("timestamp", "lux")
      )
      session$flushReact()
      expect_equal(state$plan()$datasets, "DS3")
      expect_equal(state$plan()$participants, "P3")

      fail_preview <<- TRUE
      session$setInputs(preview_rows = 25L)
      session$flushReact()
      expect_null(state$preview())
      session$setInputs(build_preview = 2L)
      session$flushReact()
      expect_equal(preview_calls, 2L)
      expect_equal(preview_limits, c(10L, 25L))
      expect_null(state$preview())
      expect_equal(state$preview_status()$state, "error")
      expect_match(state$preview_status()$message, "preview failed")

      session$setInputs(summary_continue = 1L)
      session$flushReact()
      session$setInputs(preview_continue = 1L)
      session$flushReact()
      expect_equal(selected_tabs, c("preview", "export"))

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
