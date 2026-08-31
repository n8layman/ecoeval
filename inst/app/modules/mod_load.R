# Stage 1 -- load the five inputs.
#
# Four data inputs, symmetric across the two sources, plus the schema:
#
#              paper list            record list
#   AI         papers processed      records extracted
#   Gold       papers reviewed       records recorded
#
# The paper lists are optional. They widen scope -- they do not change how
# anything is scored. Each list added lets you see a case that was previously
# invisible: the gold list brings in papers a human read and found nothing in
# (so AI over-extraction becomes visible), the AI list brings in papers the AI
# processed and found nothing in (so under-extraction does).

FILE_ROOTS <- function() {
  c(project = getwd(), home = fs::path_home())
}

file_picker <- function(ns, id, label, note = NULL, accept_tables = FALSE) {
  div(
    style = "margin-bottom: 14px;",
    tags$label(label, style = "font-size:13px; font-weight:600;"),
    if (!is.null(note)) div(class = "eco-note", note),
    div(
      style = "display:flex; gap:8px; align-items:flex-start;",
      div(style = "flex:1;",
          textInput(ns(id), NULL, width = "100%", placeholder = "path to file")),
      shinyFiles::shinyFilesButton(ns(paste0(id, "_browse")), "Browse",
                                   paste("Choose", label), multiple = FALSE)
    ),
    if (accept_tables) uiOutput(ns(paste0(id, "_table_ui")))
  )
}

mod_load_ui <- function(id) {
  ns <- NS(id)
  tagList(
    eco_panel(
      "The record sets",
      paste("Two sets of results from the same papers: one the AI produced,",
            "one a person produced by hand."),
      fluidRow(
        column(6, file_picker(ns, "ai", "AI records",
                              "An ecoextract .db, or a CSV/Excel file.",
                              accept_tables = TRUE)),
        column(6, file_picker(ns, "gold", "Gold standard records",
                              "CSV, Excel, or a database.",
                              accept_tables = TRUE))
      )
    ),
    eco_panel(
      "The schema",
      paste("Required. It drives comparator defaults, the enum conformance",
            "check, and the default linkage-field suggestion."),
      file_picker(ns, "schema", "schema.json",
                  "The schema the AI extracted against.")
    ),
    eco_panel(
      "The paper lists (optional)",
      paste("Papers processed and papers reviewed, as distinct from records",
            "found. These widen scope; they do not change how anything is",
            "scored. For an ecoextract database the AI list is free -- it is",
            "the documents table."),
      fluidRow(
        column(6, file_picker(ns, "ai_papers", "Papers the AI processed",
                              accept_tables = TRUE)),
        column(6, file_picker(ns, "gold_papers", "Papers the human reviewed",
                              accept_tables = TRUE))
      ),
      uiOutput(ns("documents_hint"))
    ),
    div(
      class = "eco-panel",
      div(style = "display:flex; gap:10px; align-items:center;",
          actionButton(ns("load"), "Load", class = "btn-primary"),
          actionButton(ns("example"), "Use the bundled example"),
          div(class = "eco-status", textOutput(ns("load_status"), inline = TRUE)))
    ),
    uiOutput(ns("report"))
  )
}

mod_load_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    inputs <- c("ai", "gold", "schema", "ai_papers", "gold_papers")

    # ---- pre-fill from run_eval_app() arguments ---------------------------
    observeEvent(rv$args, {
      for (nm in inputs) {
        v <- rv$args[[nm]]
        if (!is.null(v)) updateTextInput(session, nm, value = v)
      }
    }, once = TRUE, ignoreNULL = TRUE)

    # ---- file browsing ----------------------------------------------------
    for (nm in inputs) {
      local({
        this <- nm
        shinyFiles::shinyFileChoose(input, paste0(this, "_browse"),
                                    roots = FILE_ROOTS(), session = session)
        observeEvent(input[[paste0(this, "_browse")]], {
          sel <- shinyFiles::parseFilePaths(FILE_ROOTS(),
                                            input[[paste0(this, "_browse")]])
          if (nrow(sel)) updateTextInput(session, this, value = as.character(sel$datapath[[1]]))
        }, ignoreInit = TRUE)

        # A database needs a table chosen; a flat file does not.
        output[[paste0(this, "_table_ui")]] <- renderUI({
          path <- input[[this]]
          if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
          if (!tolower(fs::path_ext(path)) %in% c("db", "sqlite", "sqlite3")) return(NULL)
          tabs <- tryCatch(ecoeval::list_db_tables(path), error = function(e) NULL)
          if (is.null(tabs) || !nrow(tabs)) return(NULL)
          selectInput(ns(paste0(this, "_table")), "Table",
                      choices = stats::setNames(
                        tabs$table, sprintf("%s (%d rows)", tabs$table, tabs$n_rows)),
                      selected = tabs$table[[1L]], width = "100%")
        })
      })
    }

    output$documents_hint <- renderUI({
      path <- input$ai
      if (is.null(path) || !nzchar(path) ||
          !tolower(fs::path_ext(path)) %in% c("db", "sqlite", "sqlite3")) return(NULL)
      div(class = "eco-note",
          "This looks like a database. If you leave the AI paper list empty,",
          "ecoeval will use its documents table -- which knows every paper",
          "processed, including the ones that produced no records.")
    })

    # ---- the bundled example ----------------------------------------------
    observeEvent(input$example, {
      ex <- function(f) system.file("extdata", f, package = "ecoeval")
      updateTextInput(session, "ai", value = ex("ai_records.csv"))
      updateTextInput(session, "gold", value = ex("gold_records.csv"))
      updateTextInput(session, "schema", value = ex("schema.json"))
      updateTextInput(session, "ai_papers", value = ex("ai_papers.csv"))
      updateTextInput(session, "gold_papers", value = ex("gold_papers.csv"))
      showNotification(
        "Loaded the synthetic example. Press Load to read it in.",
        type = "message")
    })

    status <- reactiveVal("")
    output$load_status <- renderText(status())

    # ---- read everything --------------------------------------------------
    observeEvent(input$load, {
      status("")
      read_one <- function(nm, required = FALSE, label = nm) {
        path <- input[[nm]]
        if (is.null(path) || !nzchar(path)) {
          if (required) stop(label, " is required.", call. = FALSE)
          return(NULL)
        }
        ecoeval::read_table_any(path, input[[paste0(nm, "_table")]])
      }

      res <- tryCatch({
        schema <- ecoeval::read_schema(input$schema)
        ai <- read_one("ai", TRUE, "The AI record set")
        gold <- read_one("gold", TRUE, "The gold standard")
        ai_pap <- read_one("ai_papers")
        gold_pap <- read_one("gold_papers")

        # For a database, the documents table is the paper list for free.
        if (is.null(ai_pap) && nzchar(input$ai %||% "") &&
            tolower(fs::path_ext(input$ai)) %in% c("db", "sqlite", "sqlite3")) {
          ai_pap <- ecoeval::read_ecoextract_documents(input$ai)
        }
        list(schema = schema, ai = ai, gold = gold,
             ai_pap = ai_pap, gold_pap = gold_pap)
      }, error = function(e) e)

      if (inherits(res, "error")) {
        status(conditionMessage(res))
        showNotification(conditionMessage(res), type = "error", duration = NULL)
        return(invisible(NULL))
      }

      rv$schema <- res$schema
      rv$ai_raw <- res$ai
      rv$gold_raw <- res$gold
      rv$ai_papers_raw <- res$ai_pap
      rv$gold_papers_raw <- res$gold_pap
      rv$config$inputs <- list(
        ai = input$ai, ai_table = input$ai_table, gold = input$gold,
        gold_table = input$gold_table, schema = input$schema,
        ai_papers = input$ai_papers, gold_papers = input$gold_papers
      )
      # Loading fresh inputs invalidates everything downstream.
      rv$ai <- NULL; rv$gold <- NULL; rv$scope <- NULL; rv$paper_map <- NULL
      rv$comparators <- NULL; rv$pairs <- NULL; rv$cells <- NULL
      rv$blocked <- NULL

      status(sprintf("Read %d AI records and %d gold records against %d schema fields.",
                     nrow(res$ai), nrow(res$gold), nrow(res$schema$fields)))
      rv$stage <- "metadata"
    })

    # ---- what we read -----------------------------------------------------
    output$report <- renderUI({
      if (is.null(rv$ai_raw)) return(NULL)
      summarise_side <- function(df, papers, label) {
        div(
          style = "flex:1;",
          tags$strong(label),
          tags$ul(
            style = "font-size:12.5px; padding-left:18px; margin-top:4px;",
            tags$li(sprintf("%d records, %d columns", nrow(df), ncol(df))),
            tags$li(if (is.null(papers)) "no paper list supplied"
                    else sprintf("%d papers in the paper list", nrow(papers)))
          )
        )
      }
      eco_panel(
        "What was read",
        NULL,
        div(style = "display:flex; gap:22px;",
            summarise_side(rv$ai_raw, rv$ai_papers_raw, "AI"),
            summarise_side(rv$gold_raw, rv$gold_papers_raw, "Gold standard"))
      )
    })
  })
}
