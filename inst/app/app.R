# ecoeval -- the evaluation dashboard.
#
# This file is deliberately thin: it assembles the stage modules and holds the
# top-level reactive state they share. The scoring engine lives in the package
# (comparators.R, metrics.R, schema.R, alignment.R) as plain functions over
# data frames, so the app is a shell over a library rather than the place the
# work happens.
#
# ecoreview keeps its entire app in one 2,772-line file. That works but is
# genuinely painful to edit, and ecoeval has a natural module boundary per
# stage, so it takes it.

library(shiny)
library(shinyjs)
library(ecoeval)

for (f in list.files("modules", pattern = "[.][Rr]$", full.names = TRUE)) {
  source(f, local = FALSE)
}

STAGES <- c(
  load      = "Load the inputs",
  metadata  = "Identify the papers",
  papers    = "Align papers",
  fields    = "Map record fields",
  compare   = "Compare",
  dashboard = "Dashboard"
)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$title("ecoeval"),
    tags$link(rel = "stylesheet", type = "text/css", href = "ecoeval.css")
  ),
  div(
    class = "eco-header",
    h1("ecoeval"),
    div(class = "eco-subtitle", textOutput("subtitle", inline = TRUE))
  ),
  fluidRow(
    column(
      width = 3,
      uiOutput("rail"),
      uiOutput("scope_box"),
      uiOutput("warning_box")
    ),
    column(
      width = 9,
      uiOutput("blocker"),
      tabsetPanel(
        id = "stage", type = "hidden",
        tabPanelBody("load",      mod_load_ui("load")),
        tabPanelBody("metadata",  mod_map_metadata_ui("metadata")),
        tabPanelBody("papers",    mod_align_papers_ui("papers")),
        tabPanelBody("fields",    mod_map_records_ui("fields")),
        tabPanelBody("compare",   mod_compare_ui("compare")),
        tabPanelBody("dashboard", mod_dashboard_ui("dashboard"))
      )
    )
  )
)

server <- function(input, output, session) {
  rv <- new_app_state()

  # Anything supplied to run_eval_app() is used; the app collects the rest.
  args <- getShinyOption("ecoeval_args", list())
  rv$args <- args
  rv$normalizers <- args$normalizers
  rv$skip <- args$skip %||% character(0)
  rv$judge_mode <- args$judge_mode %||% "default"
  rv$judge <- args$judge

  restored <- NULL
  if (!is.null(args$run_config)) {
    tryCatch({
      rv$config <- ecoeval::read_run_config(args$run_config)
      restored <- ecoeval::restore_run_tables(rv$config)
      rv$rejected <- restored$rejected
      rv$added <- restored$added
      rv$overrides <- restored$overrides
      rv$reviewed <- rv$config$reviewed_papers
      rv$judge_cache <- ecoeval::new_cache(rv$config$judge_cache)
      rv$norm_cache <- ecoeval::new_cache(rv$config$normalize_cache)
      # A restored run keeps what it called the two sides, unless the launch
      # named them.
      if (!isTRUE(args$labels_given) && !is.null(rv$config$side_labels)) {
        ecoeval::use_side_labels(rv$config$side_labels)
      }
      showNotification("Restored the previous run.", type = "message")
    }, error = function(e) {
      showNotification(paste("Could not read the run configuration:",
                             conditionMessage(e)), type = "error", duration = NULL)
    })
  }

  # Run the setup stages as far as the arguments -- and a restored run -- go,
  # and open on the first one that still needs the user.
  run <- withProgress(message = "Setting up the evaluation", value = 0.5,
                      launch_state(rv, restored))
  if (!is.null(run$message) && is.null(isolate(rv$blocked))) {
    showNotification(run$message, type = "warning", duration = NULL)
  }

  mod_load_server("load", rv)
  mod_map_metadata_server("metadata", rv)
  mod_align_papers_server("papers", rv)
  mod_map_records_server("fields", rv)
  mod_compare_server("compare", rv)
  mod_dashboard_server("dashboard", rv)

  # ---- the progress rail --------------------------------------------------

  output$rail <- renderUI({
    reached <- stage_reachable(rv)
    div(
      class = "eco-rail",
      lapply(names(STAGES), function(key) {
        state <- if (identical(rv$stage, key)) "active"
                 else if (isTRUE(reached[[key]]$done)) "done"
                 else if (isTRUE(reached[[key]]$open)) "todo"
                 else "locked"
        actionButton(
          paste0("rail_", key), STAGES[[key]],
          class = paste("eco-rail-item", state),
          disabled = if (state == "locked") NA else NULL
        )
      })
    )
  })

  lapply(names(STAGES), function(key) {
    observeEvent(input[[paste0("rail_", key)]], {
      if (isTRUE(stage_reachable(rv)[[key]]$open) || identical(rv$stage, key)) {
        rv$stage <- key
      }
    }, ignoreInit = TRUE)
  })

  observeEvent(rv$stage, {
    updateTabsetPanel(session, "stage", selected = rv$stage)
  }, ignoreInit = FALSE)

  # ---- scope, warnings, and the two things that block ---------------------

  output$subtitle <- renderText({
    l <- lab()
    sprintf("How closely does %s agree with %s?", l$ai, l$gold)
  })

  output$scope_box <- renderUI({
    if (is.null(rv$scope)) return(NULL)
    div(
      class = "eco-panel",
      h3(sprintf("Scope: %d papers evaluated", length(rv$scope$papers))),
      tags$pre(
        class = "eco-status",
        sprintf("%4d  in both -- evaluated\n%4d  %s -- excluded\n%4d  %s -- excluded",
                length(rv$scope$papers),
                length(rv$scope$ai_only), paste(lab()$ai, "only"),
                length(rv$scope$gold_only), paste(lab()$gold, "only"))
      ),
      div(class = "eco-note",
          "Papers are a filter, not a scored entity. Papers outside the",
          "intersection are excluded, never penalised.")
    )
  })

  output$warning_box <- renderUI({
    w <- rv$warnings
    if (!length(w)) return(NULL)
    tagList(lapply(w, function(x) div(class = "eco-warn", x)))
  })

  output$blocker <- renderUI({
    if (is.null(rv$blocked)) return(NULL)
    div(class = "eco-block", tags$strong("Nothing to compare. "), rv$blocked)
  })

  session$onSessionEnded(function() {
    if (!interactive()) stopApp()
  })
}

shinyApp(ui, server)
