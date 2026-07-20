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

glc_explorer_status <- function(
  message,
  state = c("empty", "loading", "ready", "success", "error")
) {
  state <- match.arg(state)
  list(message = as.character(message), state = state)
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
      ".navbar .navbar-header > .navbar-brand {",
      "  display: flex;",
      "  align-items: center;",
      "  padding-block: 0;",
      "}",
      sep = "\n"
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
    registry_state <- registry_browser_server(
      "registry",
      registry = registry,
      packages = packages,
      load_registry = load_registry,
      open_package = open_package,
      schedule_after_flush = schedule_after_flush
    )
    contents_active <- shiny::reactive({
      identical(input$explorer_nav, "Package contents")
    })
    contents_state <- package_contents_server(
      "contents",
      package = registry_state$package,
      active = contents_active,
      navigation = shiny::reactive(contents_navigation()),
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
    output$package_status <- shiny::renderUI({
      glc_explorer_navbar_status_tag(summary_state$status())
    })

    list(
      registry = registry_state,
      summary = summary_state,
      contents = contents_state,
      handoff = handoff_state
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}

#' Explore Global Light Commons data packages
#'
#' Launches a local Shiny application for browsing the GLC registry, opening
#' the immutable latest passing revision of a package, reviewing its contents,
#' and filtering participants, devices, datasets, file groups, and variables.
#' The app can preview the resulting selection and export an annotated,
#' reproducible R script without uploading package data to another service.
#'
#' @param registry Optional registry JSON URL or local path. Defaults to the
#'   official registry or the value of option `glcdp.registry_url`.
#' @param launch.browser Whether to open the application in a browser.
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
  glc_assert_flag(launch.browser, "launch.browser")

  shiny::runApp(
    glc_explorer_app(registry = registry),
    launch.browser = launch.browser,
    ...
  )
}
