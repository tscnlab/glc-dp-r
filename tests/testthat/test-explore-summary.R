test_that("package summary module reports package inventory", {
  skip_if_not_installed("shiny")
  fixture <- glc_open(make_glc_fixture(), quiet = TRUE)
  pending_summary <- NULL
  scheduler <- function(callback, session) {
    pending_summary <<- callback
  }

  shiny::testServer(
    glcdp:::package_summary_server,
    args = list(
      package = shiny::reactive(fixture),
      schedule_after_flush = scheduler
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      expect_null(state$summary())
      expect_equal(state$status()$state, "loading")
      expect_true(is.function(pending_summary))

      pending_summary()
      session$flushReact()
      expect_s3_class(state$summary(), "glc_summary")
      expect_equal(state$summary()$study_count, 1L)
      expect_equal(state$summary()$dataset_count, 1L)
      expect_equal(state$summary()$participant_count, 1L)
      expect_equal(state$summary()$device_count, 1L)
      expect_equal(state$summary()$variable_count, 4L)
      expect_equal(state$status()$state, "success")

      session$setInputs(studies = 1)
      expect_equal(state$navigation()$tab, "Metadata")
      expect_equal(state$navigation()$metadata_resource, "study")

      session$setInputs(variables = 1)
      expect_equal(state$navigation()$tab, "Variables")
      expect_null(state$navigation()$metadata_resource)

      session$setInputs(load_contents = 1)
      expect_equal(
        state$contents_load()$package_key,
        glcdp:::glc_explorer_package_key(fixture)
      )
      expect_equal(state$contents_load()$request_id, 1L)
      expect_equal(state$navigation()$tab, "Variables")

      session$setInputs(open_contents = 1)
      expect_equal(state$navigation()$tab, "Metadata")
      expect_null(state$navigation()$metadata_resource)
    }
  )
})

test_that("package summary module supports schema 3 CSV metadata", {
  skip_if_not_installed("shiny")
  fixture <- glc_open(make_v3_contract_fixture(), quiet = TRUE)

  shiny::testServer(
    glcdp:::package_summary_server,
    args = list(
      package = shiny::reactive(fixture),
      schedule_after_flush = function(callback, session) callback()
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      expect_s3_class(state$summary(), "glc_summary")
      expect_equal(state$summary()$participant_count, 1L)
      expect_equal(state$summary()$variable_count, 7L)
      expect_equal(state$status()$state, "success")
    }
  )
})

test_that("package summary module handles empty and invalid packages", {
  skip_if_not_installed("shiny")

  shiny::testServer(
    glcdp:::package_summary_server,
    args = list(package = shiny::reactive(NULL)),
    {
      session$flushReact()
      state <- session$getReturned()
      expect_null(state$summary())
      expect_equal(state$status()$state, "empty")
    }
  )

  invalid <- structure(list(), class = "glc_package")
  shiny::testServer(
    glcdp:::package_summary_server,
    args = list(
      package = shiny::reactive(invalid),
      schedule_after_flush = function(callback, session) callback()
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      expect_null(state$summary())
      expect_equal(state$status()$state, "error")
      expect_match(state$status()$message, "Could not summarize")
    }
  )
})

test_that("package summary creates a safe repository link", {
  fixture <- glc_open(make_glc_fixture(), quiet = TRUE)
  fixture$repo <- "tscnlab/example-package"

  expect_equal(
    glcdp:::glc_explorer_repository_url(fixture),
    "https://github.com/tscnlab/example-package"
  )
  link <- glcdp:::glc_explorer_repository_link(fixture)
  expect_equal(link$attribs$href, "https://github.com/tscnlab/example-package")
  expect_equal(link$attribs$target, "_blank")
  expect_equal(link$attribs$rel, "noopener noreferrer")
  expect_match(as.character(link), "github", fixed = TRUE)
  expect_match(as.character(link), "Open repository in GitHub", fixed = TRUE)

  fixture$repo <- "example/package?redirect=https://invalid.example"
  expect_null(glcdp:::glc_explorer_repository_url(fixture))
  expect_null(glcdp:::glc_explorer_repository_link(fixture))
})

test_that("navbar package status is compact and semantic", {
  ready <- glcdp:::glc_explorer_navbar_status_tag(
    glcdp:::glc_explorer_status("Summary ready.", "success")
  )
  expect_match(ready$attribs$class, "text-bg-success", fixed = TRUE)
  expect_equal(ready$attribs$role, "status")
  expect_equal(ready$attribs$title, "Summary ready.")
  expect_match(as.character(ready), "Package ready", fixed = TRUE)
})

test_that("explorer exposes immediate busy feedback for reactive work", {
  busy <- as.character(glcdp:::glc_explorer_busy_indicators())

  expect_match(busy, "shinyBusySpinners = 'true'", fixed = TRUE)
  expect_match(busy, "shinyBusyPulse = 'true'", fixed = TRUE)
  expect_match(busy, "--shiny-spinner-delay:150ms", fixed = TRUE)
  expect_match(busy, "--shiny-spinner-size:2.25rem", fixed = TRUE)
  expect_match(busy, "--shiny-pulse-height:4px", fixed = TRUE)
})

test_that("global status prioritizes active loading work", {
  ready <- glcdp:::glc_explorer_status("Ready.", "success")
  loading <- glcdp:::glc_explorer_status(
    "Reading metadata and inventories\u2026",
    "loading"
  )

  status <- glcdp:::glc_explorer_global_status(
    registry = ready,
    summary = loading,
    contents = ready,
    handoff = ready,
    preview = ready,
    active = "Package summary"
  )
  expect_equal(status$source, "summary")
  expect_equal(status$title, "Building package summary")
  expect_equal(status$state, "loading")

  html <- as.character(glcdp:::glc_explorer_global_status_tag(status))
  expect_match(html, 'role="progressbar"', fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
  expect_match(html, "Building package summary", fixed = TRUE)
  expect_match(html, "Reading metadata and inventories", fixed = TRUE)

  background_contents <- glcdp:::glc_explorer_global_status(
    registry = ready,
    summary = ready,
    contents = loading,
    handoff = ready,
    preview = ready,
    active = "Package summary"
  )
  expect_equal(background_contents$source, "contents")
  expect_equal(background_contents$title, "Loading package contents")

  completed_contents <- glcdp:::glc_explorer_global_status(
    registry = ready,
    summary = ready,
    contents = glcdp:::glc_explorer_status(
      "Loaded package contents.",
      "success"
    ),
    handoff = ready,
    preview = ready,
    active = "Package summary",
    contents_requested = TRUE
  )
  expect_equal(completed_contents$source, "contents")
  expect_equal(completed_contents$title, "Package contents")
  expect_equal(completed_contents$message, "Loaded package contents.")

  active_handoff <- glcdp:::glc_explorer_global_status(
    registry = ready,
    summary = ready,
    contents = loading,
    handoff = loading,
    preview = ready,
    active = "Select & hand off",
    contents_requested = TRUE
  )
  expect_equal(active_handoff$source, "handoff")
  expect_equal(active_handoff$title, "Loading selection data")

  opening <- glcdp:::glc_explorer_global_status(
    registry = loading,
    summary = ready,
    contents = ready,
    handoff = ready,
    preview = ready,
    active = "Registry",
    opening = TRUE
  )
  expect_equal(opening$title, "Opening package")

  failed <- glcdp:::glc_explorer_global_status_tag(
    c(
      glcdp:::glc_explorer_status("Could not load.", "error"),
      list(title = "Package contents", source = "contents")
    )
  )
  expect_equal(failed$attribs$role, "alert")
  expect_equal(failed$attribs$`aria-live`, "assertive")
})

test_that("summary contents action is accessible within global status", {
  action <- glcdp:::glc_explorer_summary_contents_action(
    "summary-load_contents"
  )
  expect_equal(action$attribs$id, "summary-load_contents")
  expect_equal(action$attribs$`aria-label`, "Load package contents")
  expect_equal(
    action$attribs$title,
    "Load datasets, file groups, variables, and metadata"
  )

  retry <- glcdp:::glc_explorer_summary_contents_action(
    "summary-load_contents",
    mode = "retry"
  )
  expect_equal(
    retry$attribs$`aria-label`,
    "Retry loading package contents"
  )
  expect_equal(
    retry$attribs$title,
    "Retry loading datasets, file groups, variables, and metadata"
  )

  open <- glcdp:::glc_explorer_summary_contents_action(
    "summary-open_contents",
    mode = "open"
  )
  expect_equal(open$attribs$`aria-label`, "Open package contents")
  expect_equal(open$attribs$title, "Open the loaded package contents")

  status <- c(
    glcdp:::glc_explorer_status(
      "Package metadata and inventory summary are ready.",
      "success"
    ),
    list(title = "Package summary", source = "summary")
  )
  html <- as.character(
    glcdp:::glc_explorer_global_status_tag(status, action = action)
  )
  expect_match(html, "Load package contents", fixed = TRUE)
  expect_match(html, "summary-load_contents", fixed = TRUE)
  expect_match(html, 'aria-live="polite"', fixed = TRUE)
})

test_that("navbar title links the package logo to its documentation", {
  skip_if_not_installed("shiny")

  logo_path <- glcdp:::glc_explorer_logo_path()
  expect_true(file.exists(logo_path))

  title <- glcdp:::glc_explorer_navbar_title()
  expect_equal(title$attribs$href, "https://tscnlab.github.io/glc-dp-r/")
  expect_equal(title$attribs$target, "_blank")
  expect_equal(title$attribs$rel, "noopener noreferrer")
  expect_equal(
    title$attribs$`aria-label`,
    "Open the glcdp documentation"
  )
  html <- as.character(title)
  expect_match(html, "glcdp logo", fixed = TRUE)
  expect_match(html, "GLC data explorer", fixed = TRUE)
  expect_match(html, 'width="62"', fixed = TRUE)
  expect_match(html, 'height="62"', fixed = TRUE)
  expect_match(html, "align-items-center", fixed = TRUE)
})

test_that("summary value boxes are accessible contents links", {
  link <- glcdp:::glc_explorer_summary_link(
    "summary-studies",
    "Studies",
    2L,
    "flask",
    "Explore study metadata"
  )
  html <- as.character(link)
  expect_equal(link$attribs$id, "summary-studies")
  expect_equal(link$attribs$`aria-label`, "Explore study metadata")
  expect_match(html, "text-decoration-none", fixed = TRUE)
  expect_match(html, "value-box", fixed = TRUE)
})

test_that("summary showcase and explorer create Shiny applications", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  fixture <- glc_open(make_glc_fixture(), quiet = TRUE)

  expect_s3_class(glcdp:::package_summary_app(fixture), "shiny.appobj")
  explorer <- glcdp:::glc_explorer_app(
    packages = glcdp:::glc_explorer_registry_fixture()
  )
  expect_s3_class(explorer, "shiny.appobj")
})

test_that("explorer navigates to the summary after opening a package", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  packages <- glc_packages(make_registry_fixture())
  fixture <- glc_open(make_glc_fixture(), quiet = TRUE)
  selected_nav <- NULL
  selected_contents_nav <- NULL
  contents_load_count <- 0L
  app <- glcdp:::glc_explorer_app(
    packages = packages,
    open_package = function(row, packages, registry) fixture,
    load_contents = function(package) {
      contents_load_count <<- contents_load_count + 1L
      if (identical(contents_load_count, 1L)) {
        stop("temporary contents failure")
      }
      glcdp:::glc_explorer_load_contents(package)
    },
    schedule_after_flush = function(callback, session) callback(),
    select_nav = function(id, selected, session) {
      selected_nav <<- c(id = id, selected = selected)
    },
    select_contents_nav = function(id, selected, session) {
      selected_contents_nav <<- c(id = id, selected = selected)
    }
  )

  shiny::testServer(app$serverFuncSource(), {
    session$flushReact()
    state <- session$getReturned()
    action_id <- glcdp:::glc_explorer_registry_action_id(
      "passing",
      "example/passing"
    )
    input_id <- paste0("registry-", action_id)
    do.call(session$setInputs, stats::setNames(list(1), input_id))
    session$flushReact()

    expect_s3_class(state$package(), "glc_package")
    expect_s3_class(state$summary(), "glc_summary")
    expect_equal(
      selected_nav,
      c(id = "explorer_nav", selected = "Package summary")
    )

    session$setInputs(explorer_nav = "Package summary")
    session$flushReact()
    expect_match(
      output$global_status$html,
      "Load package contents",
      fixed = TRUE
    )
    expect_match(
      output$global_status$html,
      'id="summary-load_contents"',
      fixed = TRUE
    )

    session$setInputs(`summary-load_contents` = 1)
    session$flushReact()
    expect_equal(
      selected_nav,
      c(id = "explorer_nav", selected = "Package summary")
    )
    expect_null(selected_contents_nav)
    expect_equal(contents_load_count, 1L)
    expect_match(
      output$global_status$html,
      "Could not load package contents: temporary contents failure",
      fixed = TRUE
    )
    expect_match(
      output$global_status$html,
      "Retry loading package contents",
      fixed = TRUE
    )

    session$setInputs(`summary-load_contents` = 2)
    session$flushReact()
    expect_equal(contents_load_count, 2L)
    expect_match(
      output$global_status$html,
      "Loaded 1 datasets",
      fixed = TRUE
    )
    expect_match(
      output$global_status$html,
      "Open package contents",
      fixed = TRUE
    )

    session$setInputs(`summary-open_contents` = 1)
    session$flushReact()
    expect_equal(
      selected_nav,
      c(id = "explorer_nav", selected = "Package contents")
    )
    expect_equal(
      selected_contents_nav,
      c(id = "contents_tab", selected = "Metadata")
    )

    session$setInputs(`summary-variables` = 1)
    session$flushReact()
    expect_equal(
      selected_nav,
      c(id = "explorer_nav", selected = "Package contents")
    )
    expect_equal(
      selected_contents_nav,
      c(id = "contents_tab", selected = "Variables")
    )

    session$setInputs(
      explorer_nav = "Package contents",
      `contents-file_group_dataset_id` = "all",
      `contents-file_group_query` = "",
      `contents-file_group_device_id` = character(),
      `contents-file_group_device_location` = character(),
      `contents-file_group_location_type` = character(),
      `contents-file_group_modality` = character(),
      `contents-file_group_role` = character(),
      `contents-file_group_state` = character(),
      `contents-file_group_variable` = character(),
      `contents-file_group_term` = character()
    )
    session$flushReact()
    session$setInputs(`contents-file_group_handoff` = 1L)
    session$flushReact()

    transferred <- handoff_preselection()
    expect_named(
      transferred,
      c("package_key", "request_id", "dataset_ids", "file_group_ids")
    )
    expect_equal(
      transferred$package_key,
      glcdp:::glc_explorer_package_key(fixture)
    )
    expect_equal(transferred$request_id, 1L)
    expect_equal(transferred$dataset_ids, "DS1")
    expect_equal(transferred$file_group_ids, "DS1:1")
    expect_identical(handoff_state$handoff_mode(), "metadata")
    expect_null(handoff_state$selection())
    expect_null(handoff_state$applied_preselection())
    expect_equal(
      selected_nav,
      c(id = "explorer_nav", selected = "Select & hand off")
    )

    session$setInputs(
      explorer_nav = "Select & hand off",
      `handoff-handoff_mode` = "data"
    )
    session$flushReact()
    applied <- handoff_state$applied_preselection()
    expect_identical(handoff_state$handoff_mode(), "data")
    expect_equal(applied$request_id, "1")
    expect_equal(applied$dataset_ids, "DS1")
    expect_equal(applied$file_group_ids, "DS1:1")

    session$setInputs(`handoff-handoff_mode` = "metadata")
    session$flushReact()
    expect_identical(handoff_state$handoff_mode(), "metadata")
    expect_identical(handoff_state$plan()$mode, "metadata")
    expect_identical(handoff_state$applied_preselection(), applied)

    session$setInputs(`handoff-handoff_mode` = "data")
    session$flushReact()
    expect_identical(handoff_state$handoff_mode(), "data")
    expect_identical(handoff_state$applied_preselection(), applied)

    session$setInputs(
      `handoff-dataset_ids` = "DS1",
      `handoff-review_file_groups` = 1L
    )
    session$flushReact()

    expect_equal(contents_navigation()$tab, "File groups")
    expect_equal(contents_navigation()$dataset_ids, "DS1")
    expect_equal(
      selected_nav,
      c(id = "explorer_nav", selected = "Package contents")
    )
    expect_equal(
      selected_contents_nav,
      c(id = "contents_tab", selected = "File groups")
    )
  })
})

test_that("explorer launcher validates its arguments before running", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  expect_error(glc_explore(registry = character()), "registry")
  expect_error(glc_explore(launch.browser = NA), "launch.browser")
})

test_that("explorer launcher accepts IDE viewer functions", {
  viewer <- function(url) invisible(url)
  attr(viewer, "shinyViewerType") <- "browser"
  old_options <- options(shiny.launch.browser = viewer)
  on.exit(options(old_options), add = TRUE)

  default <- eval(
    formals(glc_explore)$launch.browser,
    envir = environment(glc_explore)
  )
  expect_identical(default, viewer)
  expect_no_error(glcdp:::glc_assert_launch_browser(default))
  expect_no_error(glcdp:::glc_assert_launch_browser(TRUE))
  expect_no_error(glcdp:::glc_assert_launch_browser(FALSE))
  expect_error(
    glcdp:::glc_assert_launch_browser(NULL),
    "function that accepts the application URL",
    fixed = TRUE
  )
})
