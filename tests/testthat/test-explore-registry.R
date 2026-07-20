test_that("registry explorer preserves registry metadata when filtering", {
  packages <- glc_packages(make_registry_fixture())

  expect_equal(
    glcdp:::glc_explorer_registry_loaded_message(packages),
    "Loaded 3 registered data packages; 1 currently passes validation."
  )

  passing <- glcdp:::glc_explorer_registry_filter(packages)
  expect_s3_class(passing, "glc_registry")
  expect_equal(passing$id, "passing")
  expect_equal(attr(passing, "source"), attr(packages, "source"))

  failed <- glcdp:::glc_explorer_registry_filter(
    packages,
    query = "example",
    status = "fail"
  )
  expect_equal(failed$id, c("older-pass", "no-pass"))

  query <- glcdp:::glc_explorer_registry_filter(
    packages,
    query = "older-pass",
    status = "all"
  )
  expect_equal(query$id, "older-pass")
})

test_that("registry table offers row actions only for passing revisions", {
  packages <- glc_packages(make_registry_fixture())
  table <- glcdp:::glc_explorer_registry_table(packages)
  expect_equal(table$can_open, c(TRUE, TRUE, FALSE))
  expect_true(all(grepl("^open_[a-f0-9]+$", table$action_id)))

  tag <- glcdp:::glc_explorer_registry_table_tag(
    packages,
    ns = shiny::NS("registry")
  )
  html <- as.character(tag)
  actions <- gregexpr('aria-label="Open the latest', html, fixed = TRUE)
  expect_equal(lengths(regmatches(html, actions)), 2L)
  expect_match(html, "No passing revision is available", fixed = TRUE)
  expect_false(grepl("table-hover", html, fixed = TRUE))
})

test_that("registry browser opens the exact selected passing revision", {
  skip_if_not_installed("shiny")
  packages <- glc_packages(make_registry_fixture())
  fixture <- glc_open(make_glc_fixture(), quiet = TRUE)
  opened_revision <- NULL
  pending_open <- NULL
  opener <- function(row, packages, registry) {
    opened_revision <<- row$latest_pass_commit[[1L]]
    fixture
  }
  scheduler <- function(callback, session) {
    pending_open <<- callback
  }

  shiny::testServer(
    glcdp:::registry_browser_server,
    args = list(
      packages = packages,
      open_package = opener,
      schedule_after_flush = scheduler
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      expect_equal(state$filtered_packages()$id, "passing")

      session$setInputs(current_status = "fail")
      action_id <- glcdp:::glc_explorer_registry_action_id(
        "older-pass",
        "example/older-pass"
      )
      do.call(session$setInputs, stats::setNames(list(1), action_id))
      expect_null(state$package())
      expect_true(state$opening())
      expect_equal(state$status()$state, "loading")
      expect_match(state$status()$message, "Opening older-pass")
      expect_true(is.function(pending_open))

      pending_open()
      session$flushReact()
      expect_s3_class(state$package(), "glc_package")
      expect_equal(
        opened_revision,
        packages$latest_pass_commit[packages$id == "older-pass"]
      )
      expect_equal(state$opened_row()$id, "older-pass")
      expect_false(state$opening())
      expect_equal(state$status()$state, "success")
    }
  )
})

test_that("registry browser reports registry and package loading failures", {
  skip_if_not_installed("shiny")
  packages <- glc_packages(make_registry_fixture())

  shiny::testServer(
    glcdp:::registry_browser_server,
    args = list(
      packages = packages,
      open_package = function(row, packages, registry) stop("opening failed"),
      schedule_after_flush = function(callback, session) callback()
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      action_id <- glcdp:::glc_explorer_registry_action_id(
        "passing",
        "example/passing"
      )
      do.call(session$setInputs, stats::setNames(list(1), action_id))
      expect_null(state$package())
      expect_equal(state$status()$state, "error")
      expect_match(state$status()$message, "opening failed", fixed = TRUE)
    }
  )

  shiny::testServer(
    glcdp:::registry_browser_server,
    args = list(
      load_registry = function(registry, refresh) stop("registry failed")
    ),
    {
      session$flushReact()
      state <- session$getReturned()
      expect_null(state$packages())
      expect_equal(state$status()$state, "error")
      expect_match(state$status()$message, "registry failed", fixed = TRUE)
    }
  )
})

test_that("registry browser showcase creates a Shiny application", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  expect_s3_class(glcdp:::registry_browser_app(), "shiny.appobj")
})
