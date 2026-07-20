glc_explorer_load_registry <- function(registry = NULL, refresh = FALSE) {
  if (is.null(registry)) {
    return(glc_packages(refresh = refresh))
  }
  glc_packages(registry = registry, refresh = refresh)
}

glc_explorer_open_latest <- function(row, packages, registry = NULL) {
  glc_open(
    row$repository[[1L]],
    ref = row$latest_pass_commit[[1L]],
    registry = packages,
    quiet = TRUE
  )
}

glc_explorer_registry_filter <- function(
  packages,
  query = "",
  status = "pass"
) {
  if (!inherits(packages, "glc_registry")) {
    glc_abort("{.arg packages} must be a `glc_registry`.")
  }
  glc_assert_string(query, "query", allow_empty = TRUE)
  glc_assert_string(status, "status")

  query <- trimws(query)
  glc_search_packages(
    query = if (nzchar(query)) query else NULL,
    packages = packages,
    status = if (identical(status, "all")) NULL else status,
    has_pass = NULL
  )
}

glc_explorer_registry_loaded_message <- function(packages) {
  total <- nrow(packages)
  passing <- sum(packages$current_status %in% "pass")
  package_noun <- if (total == 1L) "package" else "packages"
  passing_verb <- if (passing == 1L) "passes" else "pass"

  sprintf(
    "Loaded %d registered data %s; %d currently %s validation.",
    total,
    package_noun,
    passing,
    passing_verb
  )
}

glc_explorer_registry_action_id <- function(id, repository) {
  key <- paste(id, repository, sep = "\r")
  paste0(
    "open_",
    digest::digest(key, algo = "xxhash32", serialize = FALSE)
  )
}

glc_explorer_short_revision <- function(x) {
  ifelse(is.na(x) | !nzchar(x), "\u2014", substr(x, 1L, 12L))
}

glc_explorer_registry_table <- function(packages) {
  if (is.null(packages) || nrow(packages) == 0L) {
    return(data.frame(
      action_id = character(),
      can_open = logical(),
      Package = character(),
      Repository = character(),
      `Current status` = character(),
      `Latest passing revision` = character(),
      check.names = FALSE
    ))
  }
  data.frame(
    action_id = mapply(
      glc_explorer_registry_action_id,
      packages$id,
      packages$repository,
      USE.NAMES = FALSE
    ),
    can_open = packages$has_latest_pass %in% TRUE,
    Package = packages$id,
    Repository = packages$repository,
    `Current status` = ifelse(
      is.na(packages$current_status),
      "unknown",
      packages$current_status
    ),
    `Latest passing revision` = glc_explorer_short_revision(
      packages$latest_pass_commit
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

glc_explorer_registry_status_badge <- function(status) {
  status <- if (is.na(status) || !nzchar(status)) "unknown" else status
  class <- switch(
    status,
    pass = "text-bg-success",
    fail = "text-bg-danger",
    "text-bg-secondary"
  )
  shiny::tags$span(
    class = paste("badge", class),
    status
  )
}

glc_explorer_registry_table_tag <- function(
  packages,
  ns,
  opening_id = NULL
) {
  rows <- glc_explorer_registry_table(packages)
  if (nrow(rows) == 0L) {
    return(shiny::tags$p(
      class = "text-body-secondary m-3",
      "No data packages match these filters."
    ))
  }

  body <- lapply(seq_len(nrow(rows)), function(index) {
    row <- rows[index, , drop = FALSE]
    action <- if (!isTRUE(row$can_open[[1L]])) {
      shiny::tags$span(
        class = "text-body-secondary",
        title = "No passing revision is available",
        "\u2014"
      )
    } else if (identical(opening_id, row$action_id[[1L]])) {
      shiny::tags$button(
        type = "button",
        class = "btn btn-sm btn-primary",
        disabled = NA,
        shiny::icon("spinner"),
        " Opening\u2026"
      )
    } else {
      shiny::actionButton(
        ns(row$action_id[[1L]]),
        "Open",
        icon = shiny::icon("folder-open"),
        class = "btn-sm btn-primary",
        `aria-label` = paste(
          "Open the latest passing revision of",
          row$Package[[1L]]
        )
      )
    }
    shiny::tags$tr(
      shiny::tags$td(action),
      shiny::tags$td(row$Package[[1L]]),
      shiny::tags$td(row$Repository[[1L]]),
      shiny::tags$td(
        glc_explorer_registry_status_badge(
          row$`Current status`[[1L]]
        )
      ),
      shiny::tags$td(row$`Latest passing revision`[[1L]])
    )
  })

  shiny::tags$div(
    class = "table-responsive",
    shiny::tags$table(
      class = "table table-striped align-middle mb-0",
      shiny::tags$thead(
        shiny::tags$tr(
          shiny::tags$th(scope = "col", "Open"),
          shiny::tags$th(scope = "col", "Package"),
          shiny::tags$th(scope = "col", "Repository"),
          shiny::tags$th(scope = "col", "Current status"),
          shiny::tags$th(scope = "col", "Latest passing revision")
        )
      ),
      shiny::tags$tbody(body)
    )
  )
}

registry_browser_ui <- function(id) {
  ns <- shiny::NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      title = "Find a data package",
      shiny::textInput(
        ns("query"),
        "Search",
        placeholder = "Package or repository"
      ),
      shiny::selectInput(
        ns("current_status"),
        "Current validation status",
        choices = c("Pass" = "pass", "All statuses" = "all"),
        selected = "pass"
      ),
      shiny::actionButton(ns("refresh"), "Refresh registry")
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Available data packages"),
      shiny::uiOutput(ns("status_message")),
      shiny::uiOutput(ns("registry_table"))
    )
  )
}

registry_browser_server <- function(
  id,
  registry = NULL,
  packages = NULL,
  load_registry = glc_explorer_load_registry,
  open_package = glc_explorer_open_latest,
  schedule_after_flush = glc_explorer_after_flush
) {
  shiny::moduleServer(id, function(input, output, session) {
    package_registry <- shiny::reactiveVal(NULL)
    opened_package <- shiny::reactiveVal(NULL)
    opened_row <- shiny::reactiveVal(NULL)
    requested_row <- shiny::reactiveVal(NULL)
    opening <- shiny::reactiveVal(FALSE)
    opening_id <- shiny::reactiveVal(NULL)
    registered_action_ids <- character()
    status <- shiny::reactiveVal(glc_explorer_status(
      "Loading the package registry\u2026",
      "loading"
    ))

    fetch_registry <- function(refresh = FALSE) {
      status(glc_explorer_status(
        "Loading the package registry\u2026",
        "loading"
      ))
      value <- tryCatch(
        if (is.null(packages)) {
          load_registry(registry = registry, refresh = refresh)
        } else {
          packages
        },
        error = identity
      )
      if (inherits(value, "error")) {
        package_registry(NULL)
        opened_package(NULL)
        opened_row(NULL)
        requested_row(NULL)
        status(glc_explorer_status(
          paste("Could not load the registry:", conditionMessage(value)),
          "error"
        ))
        return(invisible(NULL))
      }
      if (!inherits(value, "glc_registry")) {
        package_registry(NULL)
        status(glc_explorer_status(
          "Could not load the registry: the result is not a GLC registry.",
          "error"
        ))
        return(invisible(NULL))
      }

      package_registry(value)
      opened_package(NULL)
      opened_row(NULL)
      requested_row(NULL)
      status(glc_explorer_status(
        glc_explorer_registry_loaded_message(value),
        "ready"
      ))
      invisible(value)
    }

    shiny::observeEvent(TRUE, fetch_registry(FALSE), once = TRUE)
    shiny::observeEvent(
      input$refresh,
      fetch_registry(TRUE),
      ignoreInit = TRUE
    )

    filtered_packages <- shiny::reactive({
      value <- package_registry()
      if (is.null(value)) {
        return(NULL)
      }
      glc_explorer_registry_filter(
        value,
        query = input$query %||% "",
        status = input$current_status %||% "pass"
      )
    })

    shiny::observeEvent(
      package_registry(),
      {
        value <- package_registry()
        choices <- "all"
        labels <- "All statuses"
        if (!is.null(value)) {
          statuses <- sort(unique(value$current_status))
          statuses <- statuses[!is.na(statuses) & nzchar(statuses)]
          choices <- c("all", statuses)
          labels <- c("All statuses", statuses)
        }
        previous <- shiny::isolate(input$current_status)
        selected <- if (!is.null(previous) && previous %in% choices) {
          previous
        } else if ("pass" %in% choices) {
          "pass"
        } else {
          "all"
        }
        shiny::updateSelectInput(
          session,
          "current_status",
          choices = stats::setNames(choices, labels),
          selected = selected
        )
      },
      ignoreNULL = FALSE
    )

    open_selected <- function(row, packages_for_open) {
      on.exit(
        {
          opening(FALSE)
          opening_id(NULL)
        },
        add = TRUE
      )

      value <- tryCatch(
        open_package(
          row = row,
          packages = packages_for_open,
          registry = registry
        ),
        error = identity
      )
      if (inherits(value, "error")) {
        opened_package(NULL)
        opened_row(NULL)
        status(glc_explorer_status(
          paste("Could not open the package:", conditionMessage(value)),
          "error"
        ))
        return()
      }
      if (!inherits(value, "glc_package")) {
        opened_package(NULL)
        opened_row(NULL)
        status(glc_explorer_status(
          "Could not open the package: the result is not a GLC package.",
          "error"
        ))
        return()
      }

      opened_package(value)
      opened_row(row)
      status(glc_explorer_status(
        sprintf(
          "Opened %s at %s.",
          row$id[[1L]],
          glc_explorer_short_revision(row$latest_pass_commit[[1L]])
        ),
        "success"
      ))
    }

    start_open <- function(row) {
      if (isTRUE(opening())) {
        return(invisible(NULL))
      }
      if (!isTRUE(row$has_latest_pass[[1L]])) {
        status(glc_explorer_status(
          "This package has no recorded passing revision to open.",
          "error"
        ))
        return(invisible(NULL))
      }

      action_id <- glc_explorer_registry_action_id(
        row$id[[1L]],
        row$repository[[1L]]
      )
      requested_row(row)
      status(glc_explorer_status(
        sprintf(
          "Opening %s at its latest passing revision\u2026",
          row$id[[1L]]
        ),
        "loading"
      ))
      opening(TRUE)
      opening_id(action_id)
      packages_for_open <- package_registry()
      scheduling_error <- tryCatch(
        {
          schedule_after_flush(
            function() open_selected(row, packages_for_open),
            session = session
          )
          NULL
        },
        error = identity
      )
      if (inherits(scheduling_error, "error")) {
        opening(FALSE)
        opening_id(NULL)
        status(glc_explorer_status(
          paste(
            "Could not start opening the package:",
            conditionMessage(scheduling_error)
          ),
          "error"
        ))
      }
      invisible(NULL)
    }

    register_open_actions <- function(value) {
      rows <- glc_explorer_registry_table(value)
      available <- rows$action_id[rows$can_open]
      new_ids <- setdiff(available, registered_action_ids)
      for (action_id in new_ids) {
        local({
          current_id <- action_id
          shiny::observeEvent(
            input[[current_id]],
            {
              registry_value <- package_registry()
              registry_rows <- glc_explorer_registry_table(registry_value)
              index <- which(registry_rows$action_id == current_id)
              if (length(index) != 1L) {
                status(glc_explorer_status(
                  "This registry entry is no longer available.",
                  "error"
                ))
                return()
              }
              start_open(registry_value[index, , drop = FALSE])
            },
            ignoreInit = TRUE
          )
        })
      }
      registered_action_ids <<- union(registered_action_ids, new_ids)
      invisible(NULL)
    }

    shiny::observeEvent(
      package_registry(),
      {
        value <- package_registry()
        if (!is.null(value)) {
          register_open_actions(value)
        }
      },
      ignoreNULL = TRUE
    )

    output$status_message <- shiny::renderUI({
      glc_explorer_status_tag(status())
    })
    output$registry_table <- shiny::renderUI({
      glc_explorer_registry_table_tag(
        filtered_packages(),
        ns = session$ns,
        opening_id = opening_id()
      )
    })

    list(
      packages = shiny::reactive(package_registry()),
      filtered_packages = filtered_packages,
      selected_row = shiny::reactive(requested_row()),
      package = shiny::reactive(opened_package()),
      opened_row = shiny::reactive(opened_row()),
      opening = shiny::reactive(opening()),
      opening_id = shiny::reactive(opening_id()),
      status = shiny::reactive(status())
    )
  })
}

glc_explorer_registry_fixture <- function() {
  generated_at <- "2026-01-01T00:00:00Z"
  rows <- lapply(
    list(
      list(
        id = "passing-example",
        repo = "example/passing-example",
        branch = "main",
        current = list(
          status = "pass",
          commit_sha = paste(rep("a", 40L), collapse = "")
        ),
        latest_pass = list(
          commit_sha = paste(rep("a", 40L), collapse = "")
        )
      ),
      list(
        id = "older-pass-example",
        repo = "example/older-pass-example",
        branch = "main",
        current = list(
          status = "fail",
          commit_sha = paste(rep("b", 40L), collapse = "")
        ),
        latest_pass = list(
          commit_sha = paste(rep("c", 40L), collapse = "")
        )
      )
    ),
    glc_registry_row,
    generated_at = generated_at
  )
  new_glc_registry(
    dplyr::bind_rows(rows),
    generated_at = generated_at,
    source = "showcase"
  )
}

registry_browser_app <- function(
  packages = glc_explorer_registry_fixture(),
  open_package = function(row, packages, registry) {
    new_glc_package(
      source_type = "remote",
      repo = row$repository[[1L]],
      commit = row$latest_pass_commit[[1L]],
      ref_kind = "commit",
      verified = TRUE,
      descriptor = list(resources = list()),
      schema_version = "2.0.0",
      registry_row = row
    )
  }
) {
  glc_explorer_check_dependencies()
  ui <- bslib::page_fillable(
    theme = glc_explorer_theme(),
    registry_browser_ui("registry")
  )
  server <- function(input, output, session) {
    state <- registry_browser_server(
      "registry",
      packages = packages,
      open_package = open_package
    )
    output$module_state <- shiny::renderPrint(state$status())
  }
  shiny::shinyApp(ui, server)
}
