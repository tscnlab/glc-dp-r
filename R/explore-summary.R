glc_explorer_package_details <- function(package, summary) {
  source <- if (identical(package$source_type, "local")) {
    package$root
  } else {
    package$repo
  }
  revision <- glc_scalar_character(package$commit, "Local working copy")
  verified <- if (isTRUE(package$verified)) {
    "Yes"
  } else if (identical(package$verified, FALSE)) {
    "No"
  } else {
    "Not available"
  }
  modalities <- paste(summary$modalities[[1L]], collapse = ", ")
  timezones <- paste(summary$timezones[[1L]], collapse = ", ")

  data.frame(
    Detail = c(
      "Source",
      "Revision",
      "Schema version",
      "Registry verified",
      "Modalities",
      "Time zones"
    ),
    Value = c(
      source,
      revision,
      summary$schema_version[[1L]],
      verified,
      if (nzchar(modalities)) modalities else "\u2014",
      if (nzchar(timezones)) timezones else "\u2014"
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

glc_explorer_repository_url <- function(package) {
  if (is.null(package)) {
    return(NULL)
  }
  repo <- glc_scalar_character(package$repo)
  if (
    is.na(repo) ||
      !grepl("^[A-Za-z0-9_.-]+/[A-Za-z0-9_.-]+$", repo)
  ) {
    return(NULL)
  }
  paste0("https://github.com/", repo)
}

glc_explorer_repository_link <- function(package) {
  url <- glc_explorer_repository_url(package)
  if (is.null(url)) {
    return(NULL)
  }
  shiny::tags$a(
    href = url,
    target = "_blank",
    rel = "noopener noreferrer",
    class = "btn btn-outline-primary mb-3",
    shiny::icon("github"),
    " Open repository in GitHub"
  )
}

glc_explorer_summary_box <- function(
  title,
  value,
  icon,
  theme = "primary"
) {
  bslib::value_box(
    title = title,
    value = format(value, big.mark = ",", scientific = FALSE),
    showcase = shiny::icon(icon),
    theme = theme
  )
}

glc_explorer_summary_link <- function(
  input_id,
  title,
  value,
  icon,
  destination
) {
  link <- shiny::actionLink(
    input_id,
    label = glc_explorer_summary_box(title, value, icon),
    title = destination,
    `aria-label` = destination
  )
  link$attribs$class <- paste(
    link$attribs$class,
    "d-block h-100 text-decoration-none"
  )
  link
}

package_summary_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::uiOutput(ns("value_boxes")),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Package details"),
      shiny::uiOutput(ns("repository_link")),
      shiny::tableOutput(ns("details_table"))
    )
  )
}

package_summary_server <- function(
  id,
  package,
  schedule_after_flush = glc_explorer_after_flush
) {
  if (!shiny::is.reactive(package)) {
    glc_abort("{.arg package} must be a reactive expression.")
  }

  shiny::moduleServer(id, function(input, output, session) {
    summary <- shiny::reactiveVal(NULL)
    summary_request <- 0L
    status <- shiny::reactiveVal(glc_explorer_status(
      "Open a package from the registry to inspect its summary.",
      "empty"
    ))
    navigation <- shiny::reactiveVal(NULL)
    navigation_request <- 0L

    request_contents <- function(tab, metadata_resource = NULL) {
      navigation_request <<- navigation_request + 1L
      navigation(list(
        tab = tab,
        metadata_resource = metadata_resource,
        request_id = navigation_request
      ))
      invisible(NULL)
    }

    shiny::observeEvent(
      input$studies,
      request_contents("Metadata", "study"),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$datasets,
      request_contents("Datasets"),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$participants,
      request_contents("Metadata", "participants"),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$devices,
      request_contents("Metadata", "devices"),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$files,
      request_contents("File groups"),
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$variables,
      request_contents("Variables"),
      ignoreInit = TRUE
    )

    summarize_package <- function(value, request) {
      if (!identical(request, summary_request)) {
        return()
      }
      result <- tryCatch(glc_summary(value), error = identity)
      if (inherits(result, "error")) {
        summary(NULL)
        status(glc_explorer_status(
          paste("Could not summarize the package:", conditionMessage(result)),
          "error"
        ))
        return()
      }
      summary(result)
      status(glc_explorer_status(
        "Package metadata and inventory summary are ready.",
        "success"
      ))
    }

    shiny::observeEvent(
      package(),
      {
        summary_request <<- summary_request + 1L
        request <- summary_request
        value <- package()
        if (is.null(value)) {
          summary(NULL)
          status(glc_explorer_status(
            "Open a package from the registry to inspect its summary.",
            "empty"
          ))
          return()
        }

        status(glc_explorer_status(
          "Reading the package summary\u2026",
          "loading"
        ))
        summary(NULL)
        scheduling_error <- tryCatch(
          {
            schedule_after_flush(
              function() summarize_package(value, request),
              session = session
            )
            NULL
          },
          error = identity
        )
        if (inherits(scheduling_error, "error")) {
          status(glc_explorer_status(
            paste(
              "Could not start reading the package summary:",
              conditionMessage(scheduling_error)
            ),
            "error"
          ))
        }
      },
      ignoreNULL = FALSE
    )

    output$value_boxes <- shiny::renderUI({
      value <- summary()
      if (is.null(value)) {
        return(NULL)
      }
      bslib::layout_column_wrap(
        width = 1 / 3,
        glc_explorer_summary_link(
          session$ns("studies"),
          "Studies",
          value$study_count[[1L]],
          "flask",
          "Explore study metadata"
        ),
        glc_explorer_summary_link(
          session$ns("datasets"),
          "Datasets",
          value$dataset_count[[1L]],
          "database",
          "Explore datasets"
        ),
        glc_explorer_summary_link(
          session$ns("participants"),
          "Participants",
          value$participant_count[[1L]],
          "users",
          "Explore participant metadata"
        ),
        glc_explorer_summary_link(
          session$ns("devices"),
          "Devices",
          value$device_count[[1L]],
          "microchip",
          "Explore device metadata"
        ),
        glc_explorer_summary_link(
          session$ns("files"),
          "Files",
          value$file_count[[1L]],
          "file",
          "Explore file groups and files"
        ),
        glc_explorer_summary_link(
          session$ns("variables"),
          "Variables",
          value$variable_count[[1L]],
          "list",
          "Explore variables"
        )
      )
    })
    output$repository_link <- shiny::renderUI({
      glc_explorer_repository_link(package())
    })
    output$details_table <- shiny::renderTable(
      {
        value <- summary()
        current_package <- package()
        if (is.null(value) || is.null(current_package)) {
          return(NULL)
        }
        glc_explorer_package_details(current_package, value)
      },
      rownames = FALSE,
      bordered = FALSE,
      spacing = "m"
    )

    list(
      package = package,
      summary = shiny::reactive(summary()),
      repository_url = shiny::reactive(
        glc_explorer_repository_url(package())
      ),
      navigation = shiny::reactive(navigation()),
      status = shiny::reactive(status())
    )
  })
}

package_summary_app <- function(package) {
  glc_explorer_check_dependencies()
  if (!inherits(package, "glc_package")) {
    glc_abort("{.arg package} must be opened with {.fn glc_open}.")
  }
  ui <- bslib::page_fluid(
    theme = glc_explorer_theme(),
    package_summary_ui("summary"),
    bslib::card(
      bslib::card_header("Development status"),
      shiny::verbatimTextOutput("module_state")
    )
  )
  server <- function(input, output, session) {
    current_package <- shiny::reactive(package)
    state <- package_summary_server("summary", current_package)
    output$module_state <- shiny::renderPrint(state$status())
  }
  shiny::shinyApp(ui, server)
}
