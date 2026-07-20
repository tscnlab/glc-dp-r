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
      expect_equal(state$summary()$variable_count, 2L)
      expect_equal(state$status()$state, "success")

      session$setInputs(studies = 1)
      expect_equal(state$navigation()$tab, "Metadata")
      expect_equal(state$navigation()$metadata_resource, "study")

      session$setInputs(variables = 1)
      expect_equal(state$navigation()$tab, "Variables")
      expect_null(state$navigation()$metadata_resource)
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
  expect_s3_class(
    glcdp:::glc_explorer_app(
      packages = glcdp:::glc_explorer_registry_fixture()
    ),
    "shiny.appobj"
  )
})

test_that("explorer navigates to the summary after opening a package", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  packages <- glc_packages(make_registry_fixture())
  fixture <- glc_open(make_glc_fixture(), quiet = TRUE)
  selected_nav <- NULL
  selected_contents_nav <- NULL
  app <- glcdp:::glc_explorer_app(
    packages = packages,
    open_package = function(row, packages, registry) fixture,
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
  })
})

test_that("explorer launcher validates its arguments before running", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  expect_error(glc_explore(registry = character()), "registry")
  expect_error(glc_explore(launch.browser = NA), "launch.browser")
})
