# Stage 2 -- map the metadata fields.
#
# This has to precede paper alignment: before we can decide which AI paper is
# which gold paper, we need to know which column holds the DOI, the title, the
# year. Paper alignment determines scope, not score -- but its asymmetry is
# worth knowing. A missed paper match just shrinks the evaluation set; a wrong
# paper match compares one paper's records against another's and produces
# garbage.

META_ROLES <- c(doi = "DOI / identifier", title = "Title",
                author = "First author", year = "Year")

mod_map_metadata_ui <- function(id) {
  ns <- NS(id)
  tagList(
    eco_panel(
      "Which column identifies the paper?",
      paste("Every record has to be attributable to a paper before anything",
            "can be compared. A DOI is ideal; anything stable will do, as long",
            "as the two sources use the same kind of value."),
      fluidRow(
        column(6, uiOutput(ns("ai_paper_col"))),
        column(6, uiOutput(ns("gold_paper_col")))
      )
    ),
    uiOutput(ns("paper_list_cols")),
    uiOutput(ns("metadata_roles")),
    div(class = "eco-panel",
        div(style = "display:flex; gap:10px; align-items:center;",
            actionButton(ns("apply"), "Apply and continue", class = "btn-primary"),
            div(class = "eco-status", textOutput(ns("status"), inline = TRUE))))
  )
}

mod_map_metadata_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    col_selector <- function(inputId, label, df, selected = NULL) {
      if (is.null(df)) return(NULL)
      selectInput(ns(inputId), label, choices = names(df),
                  selected = selected %||% ecoeval::suggest_paper_column(df),
                  width = "100%")
    }

    output$ai_paper_col <- renderUI({
      col_selector("ai_paper_col", "AI records", rv$ai_raw)
    })
    output$gold_paper_col <- renderUI({
      col_selector("gold_paper_col", "Gold standard records", rv$gold_raw)
    })

    output$paper_list_cols <- renderUI({
      if (is.null(rv$ai_papers_raw) && is.null(rv$gold_papers_raw)) return(NULL)
      eco_panel(
        "Which column identifies the paper in the paper lists?",
        NULL,
        fluidRow(
          column(6, col_selector("ai_papers_col", "AI paper list", rv$ai_papers_raw)),
          column(6, col_selector("gold_papers_col", "Gold paper list", rv$gold_papers_raw))
        )
      )
    })

    output$metadata_roles <- renderUI({
      if (is.null(rv$ai_papers_raw) || is.null(rv$gold_papers_raw)) return(NULL)
      common <- intersect(squash_ci(names(rv$ai_papers_raw)),
                          squash_ci(names(rv$gold_papers_raw)))
      eco_panel(
        "Metadata to align papers on (optional)",
        paste("Used only when the two identifier columns do not line up",
              "exactly. When they do, identifiers alone are enough and this",
              "can be left empty."),
        fluidRow(
          column(6, selectInput(
            ns("ai_meta"), "AI paper columns",
            choices = names(rv$ai_papers_raw), multiple = TRUE,
            selected = intersect(names(rv$ai_papers_raw), c("title", "year", "author")),
            width = "100%")),
          column(6, selectInput(
            ns("gold_meta"), "Gold paper columns",
            choices = names(rv$gold_papers_raw), multiple = TRUE,
            selected = intersect(names(rv$gold_papers_raw), c("title", "year", "author")),
            width = "100%"))
        ),
        if (!length(common)) div(class = "eco-note",
            "The two paper lists share no column names, so alignment will fall",
            "back to the identifiers.")
      )
    })

    status <- reactiveVal("")
    output$status <- renderText(status())

    observeEvent(input$apply, {
      req(rv$ai_raw, rv$gold_raw)
      res <- tryCatch({
        rv$ai_paper_col <- input$ai_paper_col
        rv$gold_paper_col <- input$gold_paper_col

        # Records keep every column at this stage; the record-field mapping in
        # stage 4 decides which of them are actually compared.
        rv$ai <- ecoeval::prepare_records(rv$ai_raw, input$ai_paper_col, prefix = "a")
        rv$gold <- ecoeval::prepare_records(rv$gold_raw, input$gold_paper_col,
                                            prefix = "g")

        meta_names <- function(cols) {
          if (!length(cols)) return(NULL)
          stats::setNames(cols, squash_ci(cols))
        }
        rv$ai_papers <- if (!is.null(rv$ai_papers_raw)) {
          ecoeval::prepare_papers(rv$ai_papers_raw, input$ai_papers_col,
                                  meta_names(input$ai_meta))
        }
        rv$gold_papers <- if (!is.null(rv$gold_papers_raw)) {
          ecoeval::prepare_papers(rv$gold_papers_raw, input$gold_papers_col,
                                  meta_names(input$gold_meta))
        }
        rv$metadata_fields <- intersect(squash_ci(input$ai_meta %||% character(0)),
                                        squash_ci(input$gold_meta %||% character(0)))
        TRUE
      }, error = function(e) e)

      if (inherits(res, "error")) {
        status(conditionMessage(res))
        showNotification(conditionMessage(res), type = "error", duration = NULL)
        return(invisible(NULL))
      }

      # Internal consistency, run at load: every paper referenced in a record
      # list should appear in that source's paper list.
      checks <- dplyr::bind_rows(
        ecoeval::source_consistency(rv$ai, rv$ai_papers, "ai"),
        ecoeval::source_consistency(rv$gold, rv$gold_papers, "gold")
      )
      rv$warnings <- unique(c(
        setdiff(rv$warnings, checks$detail),
        if (nrow(checks)) paste0(toupper(checks$source), ": ", checks$detail)
      ))
      status(sprintf("%d AI records and %d gold records placed.",
                     nrow(rv$ai), nrow(rv$gold)))
      rv$stage <- "papers"
    })
  })
}

squash_ci <- function(x) {
  gsub("[^a-z0-9]", "", tolower(as.character(x)))
}
