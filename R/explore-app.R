glc_explorer_dependencies <- function() {
  c("bslib", "shiny")
}

glc_explorer_check_dependencies <- function() {
  missing <- glc_explorer_dependencies()[
    !vapply(
      glc_explorer_dependencies(),
      requireNamespace,
      logical(1),
      quietly = TRUE
    )
  ]
  if (length(missing) > 0L) {
    packages <- paste(sprintf("`%s`", missing), collapse = ", ")
    glc_abort(
      paste0(
        "The GLC data explorer requires the suggested package(s) ",
        packages,
        ". Install them before launching the app."
      ),
      class = "glcdp_missing_app_dependency"
    )
  }
  invisible(TRUE)
}

glc_assert_launch_browser <- function(value, arg = "launch.browser") {
  if (is.function(value)) {
    return(invisible(value))
  }
  valid <- is.logical(value) &&
    length(value) == 1L &&
    !is.na(value)
  if (!valid) {
    glc_abort(
      "{.arg {arg}} must be `TRUE`, `FALSE`, or a function that accepts the application URL."
    )
  }
  invisible(value)
}

glc_explorer_status <- function(
  message,
  state = c("empty", "loading", "ready", "success", "error")
) {
  state <- match.arg(state)
  list(message = as.character(message), state = state)
}

glc_explorer_global_status <- function(
  registry,
  summary,
  contents,
  handoff,
  preview,
  active = "Registry",
  opening = FALSE,
  contents_requested = FALSE
) {
  statuses <- list(
    registry = registry,
    summary = summary,
    contents = contents,
    handoff = handoff,
    preview = preview
  )
  active <- active %||% "Registry"
  active_source <- switch(
    active,
    `Package summary` = "summary",
    `Package contents` = "contents",
    `Select & hand off` = if (preview$state %in% c("loading", "error")) {
      "preview"
    } else {
      "handoff"
    },
    "registry"
  )
  loading_sources <- c("registry", "summary", "contents")
  if (identical(active, "Select & hand off")) {
    loading_sources <- c(
      "handoff",
      "preview",
      "registry",
      "summary",
      "contents"
    )
  }
  loading <- loading_sources[vapply(
    statuses[loading_sources],
    function(status) identical(status$state, "loading"),
    logical(1)
  )]
  source <- if (length(loading) > 0L) {
    loading[[1L]]
  } else if (identical(registry$state, "error")) {
    "registry"
  } else if (
    identical(contents$state, "error") &&
      active %in% c("Package summary", "Package contents")
  ) {
    "contents"
  } else if (
    identical(active, "Package summary") &&
      isTRUE(contents_requested) &&
      identical(contents$state, "success")
  ) {
    "contents"
  } else {
    active_source
  }

  loading_titles <- list(
    registry = if (isTRUE(opening)) {
      "Opening package"
    } else {
      "Loading package registry"
    },
    summary = "Building package summary",
    contents = "Loading package contents",
    handoff = "Loading selection data",
    preview = "Building data preview"
  )
  titles <- list(
    registry = "Package registry",
    summary = "Package summary",
    contents = "Package contents",
    handoff = "Select and hand off",
    preview = "Data preview"
  )
  status <- statuses[[source]]
  title <- if (identical(status$state, "loading")) {
    loading_titles[[source]]
  } else {
    titles[[source]]
  }
  c(status, list(title = title, source = source))
}

glc_explorer_status_tag <- function(status) {
  classes <- c(
    empty = "alert alert-secondary",
    loading = "alert alert-info",
    ready = "alert alert-secondary",
    success = "alert alert-success",
    error = "alert alert-danger"
  )
  shiny::tags$div(
    class = unname(classes[[status$state]]),
    role = "status",
    status$message
  )
}

glc_explorer_global_status_tag <- function(status, action = NULL) {
  config <- switch(
    status$state,
    empty = list(class = "alert-secondary", icon = "circle"),
    loading = list(class = "alert-info", icon = "hourglass-half"),
    ready = list(class = "alert-secondary", icon = "circle-info"),
    success = list(class = "alert-success", icon = "circle-check"),
    error = list(class = "alert-danger", icon = "triangle-exclamation")
  )
  progress <- if (identical(status$state, "loading")) {
    shiny::tags$div(
      class = "progress mt-2",
      style = "height: 0.55rem;",
      role = "progressbar",
      `aria-label` = status$title,
      `aria-valuetext` = "In progress",
      shiny::tags$div(
        class = paste(
          "progress-bar progress-bar-striped",
          "progress-bar-animated bg-primary w-100"
        )
      )
    )
  }
  shiny::tags$section(
    class = paste(
      "alert",
      config$class,
      "shadow-sm mb-0"
    ),
    role = if (identical(status$state, "error")) "alert" else "status",
    `aria-live` = if (identical(status$state, "error")) {
      "assertive"
    } else {
      "polite"
    },
    `aria-atomic` = "true",
    shiny::tags$div(
      class = "d-flex align-items-start gap-3",
      shiny::tags$div(
        class = "fs-4 lh-1 flex-shrink-0",
        shiny::icon(config$icon)
      ),
      shiny::tags$div(
        class = "flex-grow-1",
        shiny::tags$div(class = "fw-semibold", status$title),
        shiny::tags$div(status$message),
        progress,
        if (!is.null(action)) {
          shiny::tags$div(class = "mt-3", action)
        }
      )
    )
  )
}

glc_explorer_navbar_status_tag <- function(status) {
  config <- switch(
    status$state,
    empty = list(
      class = "text-bg-secondary",
      icon = "circle",
      label = "No package open"
    ),
    loading = list(
      class = "text-bg-info",
      icon = "hourglass-half",
      label = "Loading package"
    ),
    ready = list(
      class = "text-bg-secondary",
      icon = "circle",
      label = "Package selected"
    ),
    success = list(
      class = "text-bg-success",
      icon = "circle-check",
      label = "Package ready"
    ),
    error = list(
      class = "text-bg-danger",
      icon = "triangle-exclamation",
      label = "Package error"
    )
  )
  shiny::tags$span(
    class = paste("badge rounded-pill", config$class),
    role = "status",
    title = status$message,
    shiny::icon(config$icon),
    paste0(" ", config$label)
  )
}

glc_explorer_after_flush <- function(callback, session) {
  if (!is.function(callback)) {
    glc_abort("{.arg callback} must be a function.")
  }
  session$onFlushed(callback, once = TRUE)
  invisible(NULL)
}

glc_explorer_theme <- function() {
  theme <- bslib::bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#005293"
  )
  bslib::bs_add_rules(
    theme,
    paste(
      c(
        ".navbar .navbar-header > .navbar-brand {",
        "  display: flex;",
        "  align-items: center;",
        "  padding-block: 0;",
        "}",
        ".metadata-values-grid {",
        "  display: grid;",
        "  grid-template-columns: max-content minmax(12rem, 1fr);",
        "  column-gap: 0.75rem;",
        "  row-gap: 0.35rem;",
        "  margin-left: 0.75rem !important;",
        "}",
        ".metadata-record-values {",
        "  overflow-x: auto;",
        "}",
        ".metadata-values-grid > dt {",
        "  grid-column: 1;",
        "  margin: 0;",
        "  white-space: nowrap;",
        "}",
        ".metadata-values-grid > dt code {",
        "  white-space: nowrap;",
        "  overflow-wrap: normal;",
        "  word-break: normal;",
        "}",
        ".metadata-values-grid > dd {",
        "  grid-column: 2;",
        "  margin: 0;",
        "  min-width: 0;",
        "}",
        ".metadata-repeated-field {",
        "  margin-left: 0.75rem !important;",
        "}",
        ".metadata-repeated-field > summary {",
        "  display: flex;",
        "  align-items: center;",
        "  gap: 0.4rem;",
        "  cursor: pointer;",
        "}",
        ".metadata-repeated-field > summary code {",
        "  white-space: nowrap;",
        "  overflow-wrap: normal;",
        "  word-break: normal;",
        "}",
        ".metadata-disclosure-arrow {",
        "  display: inline-flex;",
        "  transition: transform 0.15s ease-in-out;",
        "}",
        ".metadata-repeated-field[open] > summary .metadata-disclosure-arrow {",
        "  transform: rotate(90deg);",
        "}",
        ".metadata-repeated-values {",
        "  margin-left: 0.75rem;",
        "  padding-left: 1.25rem;",
        "}",
        ".metadata-lazy-output.recalculating {",
        "  min-height: 4rem;",
        "}",
        ".metadata-lazy-placeholder {",
        "  display: none;",
        "}",
        paste0(
          ".metadata-lazy-output.recalculating + ",
          ".metadata-lazy-placeholder {"
        ),
        "  display: block;",
        "}",
        ".handoff-wizard .nav {",
        "  flex-wrap: wrap;",
        "}",
        ".handoff-wizard .nav-link {",
        "  white-space: nowrap;",
        "  padding-inline: 0.7rem;",
        "  font-size: 0.95rem;",
        "}",
        ".handoff-wizard-step {",
        "  width: 100%;",
        "  height: 100%;",
        "  min-height: 0;",
        "  flex: 1 1 auto;",
        "  display: flex;",
        "  flex-direction: column;",
        "  overflow: hidden;",
        "}",
        ".handoff-wizard-step > .shiny-panel-conditional {",
        "  flex: 1 1 auto;",
        "  min-height: 0;",
        "  height: 100%;",
        "}",
        ".handoff-step-stack {",
        "  flex: 1 1 auto;",
        "  display: flex;",
        "  flex-direction: column;",
        "  height: 100%;",
        "  min-height: 0;",
        "  overflow: hidden;",
        "}",
        ".handoff-step-content {",
        "  flex: 1 1 auto;",
        "  min-height: 0;",
        "  overflow-y: auto;",
        "  padding-right: 0.25rem;",
        "}",
        ".handoff-step-nav-region {",
        "  flex: 0 0 auto;",
        "  min-height: 0;",
        "}",
        ".handoff-step-nav-region > .shiny-panel-conditional {",
        "  width: 100%;",
        "}",
        ".handoff-wizard-step-form {",
        "  max-width: none;",
        "}",
        ".handoff-wizard-layout {",
        "  display: grid;",
        "  gap: 1rem;",
        "  align-items: stretch;",
        "  min-height: 0;",
        "}",
        ".handoff-control-column,",
        ".handoff-information-column {",
        "  min-width: 0;",
        "  min-height: 0;",
        "}",
        ".handoff-information-column {",
        "  padding: 0.25rem;",
        "}",
        ".handoff-information-scroll {",
        "  max-height: 68vh;",
        "  overflow: auto;",
        "}",
        ".handoff-control-grid {",
        "  display: grid;",
        "  grid-template-columns: minmax(0, 1fr);",
        "  gap: 0.75rem 1rem;",
        "}",
        ".handoff-control-block {",
        "  min-width: 0;",
        "}",
        ".handoff-step-nav {",
        "  display: grid;",
        "  grid-template-columns: repeat(2, minmax(0, 1fr));",
        "  gap: 0.75rem;",
        "  flex: 0 0 auto;",
        "  margin-top: 0.75rem;",
        "  padding-top: 0.75rem;",
        "  border-top: 1px solid var(--bs-border-color);",
        "  background: var(--bs-body-bg);",
        "  position: relative;",
        "  z-index: 2;",
        "}",
        ".handoff-step-nav-slot {",
        "  min-width: 0;",
        "}",
        ".handoff-step-nav-slot > .shiny-panel-conditional {",
        "  width: 100%;",
        "}",
        ".handoff-nav-action,",
        ".handoff-primary-action {",
        "  width: 100%;",
        "  min-height: 2.75rem;",
        "}",
        ".handoff-bounded-selectize .selectize-input.items {",
        "  max-height: 7.5rem;",
        "  overflow-y: auto;",
        "  overscroll-behavior: contain;",
        "  align-content: flex-start;",
        "}",
        ".handoff-bounded-selectize .selectize-input > .item {",
        "  max-width: 100%;",
        "  overflow: hidden;",
        "  text-overflow: ellipsis;",
        "  white-space: nowrap;",
        "}",
        ".handoff-inline-result {",
        "  min-width: 0;",
        "}",
        ".handoff-script-result pre {",
        "  margin-bottom: 0;",
        "}",
        ".handoff-review-accordion {",
        "  margin-bottom: 0.75rem;",
        "}",
        "@media (min-width: 992px) {",
        "  .handoff-wizard {",
        "    height: 100%;",
        "  }",
        "  .handoff-wizard-layout {",
        "    height: calc(100dvh - 12rem);",
        "    min-height: 22rem;",
        paste0(
          "    grid-template-columns: minmax(0, 3fr) ",
          "minmax(21rem, 2fr);"
        ),
        "  }",
        "  .handoff-control-column > .card {",
        "    height: 100%;",
        "    min-height: 0;",
        "    overflow: hidden;",
        "  }",
        "  .handoff-control-column .tab-content,",
        "  .handoff-control-column .tab-pane {",
        "    height: 100%;",
        "    min-height: 0;",
        "    overflow: hidden;",
        "  }",
        "  .handoff-control-column .card-body {",
        "    min-height: 0;",
        "    overflow: hidden;",
        "    margin-block: 0 !important;",
        "  }",
        "  .handoff-information-column {",
        "    position: sticky;",
        "    top: 0;",
        "    height: 100%;",
        "    overflow-y: auto;",
        "  }",
        "  .handoff-information-scroll {",
        "    max-height: none;",
        "  }",
        "}",
        "@media (min-width: 1200px) {",
        "  .handoff-control-grid {",
        "    grid-template-columns: repeat(2, minmax(0, 1fr));",
        "  }",
        "  .handoff-control-span {",
        "    grid-column: 1 / -1;",
        "  }",
        "}"
      ),
      collapse = "\n"
    )
  )
}

glc_explorer_busy_indicators <- function() {
  shiny::tagList(
    shiny::useBusyIndicators(
      spinners = TRUE,
      pulse = TRUE,
      fade = TRUE
    ),
    shiny::busyIndicatorOptions(
      spinner_delay = "150ms",
      spinner_size = "2.25rem",
      pulse_height = "4px"
    )
  )
}

glc_explorer_logo_path <- function() {
  candidates <- c(
    system.file("help", "figures", "logo.png", package = "glcdp"),
    system.file("man", "figures", "logo.png", package = "glcdp")
  )
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (length(candidates) == 0L) {
    return(NULL)
  }
  candidates[[1L]]
}

glc_explorer_navbar_title <- function() {
  label <- shiny::tags$span(
    class = "d-flex align-items-center align-self-stretch",
    "GLC data explorer"
  )
  logo_path <- glc_explorer_logo_path()
  if (is.null(logo_path)) {
    return(label)
  }

  shiny::addResourcePath(
    "glcdp-explorer-assets",
    dirname(logo_path)
  )
  shiny::tags$a(
    href = "https://tscnlab.github.io/glc-dp-r/",
    target = "_blank",
    rel = "noopener noreferrer",
    class = paste(
      "d-flex align-items-center gap-2",
      "text-decoration-none text-reset"
    ),
    title = "Open the glcdp documentation",
    `aria-label` = "Open the glcdp documentation",
    shiny::tags$img(
      src = "glcdp-explorer-assets/logo.png",
      alt = "glcdp logo",
      class = "d-block flex-shrink-0",
      width = 62,
      height = 62
    ),
    label
  )
}

glc_explorer_app <- function(
  registry = NULL,
  packages = NULL,
  load_registry = glc_explorer_load_registry,
  open_package = glc_explorer_open_latest,
  load_contents = glc_explorer_load_contents,
  schedule_after_flush = glc_explorer_after_flush,
  select_nav = bslib::nav_select,
  select_contents_nav = bslib::nav_select
) {
  glc_explorer_check_dependencies()

  ui <- bslib::page_navbar(
    title = glc_explorer_navbar_title(),
    id = "explorer_nav",
    theme = glc_explorer_theme(),
    fillable = c("Registry", "Package contents", "Select & hand off"),
    fillable_mobile = TRUE,
    header = shiny::tagList(
      glc_explorer_busy_indicators(),
      shiny::tags$div(
        class = "container py-3",
        shiny::uiOutput("global_status")
      )
    ),
    bslib::nav_panel(
      "Registry",
      registry_browser_ui("registry")
    ),
    bslib::nav_panel(
      "Package summary",
      package_summary_ui("summary")
    ),
    bslib::nav_panel(
      "Package contents",
      package_contents_ui("contents")
    ),
    bslib::nav_panel(
      "Select & hand off",
      selection_handoff_ui("handoff")
    ),
    bslib::nav_spacer(),
    bslib::nav_item(
      shiny::uiOutput("package_status", inline = TRUE)
    )
  )

  server <- function(input, output, session) {
    contents_navigation <- shiny::reactiveVal(NULL)
    handoff_preselection <- shiny::reactiveVal(NULL)
    registry_state <- registry_browser_server(
      "registry",
      registry = registry,
      packages = packages,
      load_registry = load_registry,
      open_package = open_package,
      schedule_after_flush = schedule_after_flush
    )
    summary_state <- NULL
    contents_active <- shiny::reactive({
      identical(input$explorer_nav, "Package contents") ||
        (!is.null(summary_state) && !is.null(summary_state$contents_load()))
    })
    contents_state <- package_contents_server(
      "contents",
      package = registry_state$package,
      active = contents_active,
      navigation = shiny::reactive(contents_navigation()),
      load_contents = load_contents,
      schedule_after_flush = schedule_after_flush,
      select_nav = select_contents_nav
    )
    handoff_active <- shiny::reactive({
      identical(input$explorer_nav, "Select & hand off")
    })
    handoff_state <- selection_handoff_server(
      "handoff",
      package = registry_state$package,
      active = handoff_active,
      preselection = shiny::reactive(handoff_preselection()),
      default_mode = "metadata",
      schedule_after_flush = schedule_after_flush
    )
    summary_state <- package_summary_server(
      "summary",
      registry_state$package,
      schedule_after_flush = schedule_after_flush
    )
    shiny::observeEvent(
      registry_state$package(),
      select_nav(
        id = "explorer_nav",
        selected = "Package summary",
        session = session
      ),
      ignoreInit = TRUE,
      priority = 100
    )
    shiny::observeEvent(
      summary_state$navigation(),
      {
        contents_navigation(summary_state$navigation())
        select_nav(
          id = "explorer_nav",
          selected = "Package contents",
          session = session
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      priority = 100
    )
    shiny::observeEvent(
      contents_state$handoff_request(),
      {
        handoff_preselection(contents_state$handoff_request())
        select_nav(
          id = "explorer_nav",
          selected = "Select & hand off",
          session = session
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      priority = 100
    )
    shiny::observeEvent(
      handoff_state$contents_request(),
      {
        contents_navigation(handoff_state$contents_request())
        select_nav(
          id = "explorer_nav",
          selected = "Package contents",
          session = session
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE,
      priority = 100
    )
    output$package_status <- shiny::renderUI({
      glc_explorer_navbar_status_tag(summary_state$status())
    })
    global_status <- shiny::reactive({
      glc_explorer_global_status(
        registry = registry_state$status(),
        summary = summary_state$status(),
        contents = contents_state$status(),
        handoff = handoff_state$status(),
        preview = handoff_state$preview_status(),
        active = input$explorer_nav,
        opening = registry_state$opening(),
        contents_requested = !is.null(summary_state$contents_load())
      )
    })
    output$global_status <- shiny::renderUI({
      action <- NULL
      if (
        identical(input$explorer_nav, "Package summary") &&
          identical(summary_state$status()$state, "success")
      ) {
        contents_status <- contents_state$status()
        action <- if (identical(contents_status$state, "ready")) {
          glc_explorer_summary_contents_action(
            shiny::NS("summary", "load_contents")
          )
        } else if (identical(contents_status$state, "error")) {
          glc_explorer_summary_contents_action(
            shiny::NS("summary", "load_contents"),
            mode = "retry"
          )
        } else if (
          identical(contents_status$state, "success") &&
            !is.null(summary_state$contents_load())
        ) {
          glc_explorer_summary_contents_action(
            shiny::NS("summary", "open_contents"),
            mode = "open"
          )
        }
      }
      glc_explorer_global_status_tag(global_status(), action = action)
    })

    list(
      registry = registry_state,
      summary = summary_state,
      contents = contents_state,
      handoff = handoff_state,
      contents_navigation = shiny::reactive(contents_navigation()),
      handoff_preselection = shiny::reactive(handoff_preselection()),
      global_status = global_status
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}

#' Explore Global Light Commons data packages
#'
#' Launches a local Shiny application for browsing the GLC registry, opening
#' the immutable latest passing revision of a package, reviewing its contents,
#' and filtering participants, devices, datasets, file groups, semantic terms,
#' and source variables. The app can preview the resulting selection and
#' export an annotated, reproducible R script without uploading package data
#' to another service.
#'
#' @param registry Optional registry JSON URL or local path. Defaults to the
#'   official registry or the value of option `glcdp.registry_url`.
#' @param launch.browser Whether to open the application in a browser, or a
#'   function that Shiny calls with the application URL. The default respects
#'   IDE viewer functions supplied through `shiny.launch.browser`.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return Called for its side effect of running a Shiny application.
#' @seealso The [Shiny app workflow](../articles/glc-data-explorer.html).
#' @export
#'
#' @examplesIf interactive()
#' glc_explore()
glc_explore <- function(
  registry = NULL,
  launch.browser = getOption("shiny.launch.browser", interactive()),
  ...
) {
  glc_explorer_check_dependencies()
  if (!is.null(registry)) {
    glc_assert_string(registry, "registry")
  }
  glc_assert_launch_browser(launch.browser)

  shiny::runApp(
    glc_explorer_app(registry = registry),
    launch.browser = launch.browser,
    ...
  )
}
