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
  expect_match(hierarchy_html, "record 1 — P1", fixed = TRUE)
  expect_match(hierarchy_html, "participant_internal_id", fixed = TRUE)
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
  expect_match(
    as.character(
      glcdp:::glc_explorer_metadata_hierarchy_tag(participant_hierarchy)
    ),
    "record 1 — P001",
    fixed = TRUE
  )

  datasheets <- glcdp:::glc_explorer_filter_metadata(
    metadata,
    resource = "device_datasheets",
    query = "datasheet_model"
  )
  expect_equal(
    glcdp:::glc_explorer_metadata_table(datasheets)$`Record ID`,
    c("sheet-a", "sheet-b")
  )
  expect_match(
    datasheet_hierarchy <- as.character(
      glcdp:::glc_explorer_metadata_hierarchy_tag(datasheets)
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
    glcdp:::glc_explorer_metadata_hierarchy_tag(all_datasheets)
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

test_that("package contents UI offers multi-dataset selectize controls", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  html <- as.character(glcdp:::package_contents_ui("contents"))
  multiple <- regmatches(
    html,
    gregexpr('multiple="multiple"', html, fixed = TRUE)
  )[[1L]]

  expect_length(multiple, 2L)
  expect_match(html, "All datasets", fixed = TRUE)
  expect_match(html, "contents-file_group_select_all", fixed = TRUE)
  expect_match(html, "contents-variable_select_all", fixed = TRUE)
  expect_match(html, "contents-file_group_clear", fixed = TRUE)
  expect_match(html, "contents-variable_clear", fixed = TRUE)
  expect_match(html, 'placeholder="Search datasets"', fixed = TRUE)
  expect_match(
    html,
    paste(
      "Search by dataset ID, study ID, participant ID,",
      "device ID, or modality."
    ),
    fixed = TRUE
  )
  expect_match(
    html,
    paste(
      "Use Hierarchy to find a field, search for its field name,",
      "then switch to Table to compare it across all records."
    ),
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

      session$setInputs(
        file_group_dataset_id = "DS2",
        file_group_query = "light",
        variable_dataset_id = "DS2",
        variable_query = "quality",
        primary_only = FALSE,
        metadata_resource = "participants",
        metadata_query = "P1"
      )
      expect_equal(state$file_groups()$file_group_id, "DS2:1")
      expect_equal(state$variables()$dataset_id, "DS2")
      expect_equal(state$variables()$name, "quality")
      expect_true(all(state$metadata()$resource == "participants"))

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
