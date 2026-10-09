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

# What the path box shows for an input handed to run_eval_app() as an object
# -- a data frame, or a schema already read -- rather than a path.
FRAME_PLACEHOLDER <- "(supplied from R)"

# The caller's directory, not the app's: runApp() moves into the app folder.
FILE_ROOTS <- function(project = NULL) {
  c(project = project %||% getwd(), home = fs::path_home())
}

# A path typed into a box is relative to the caller's directory.
resolve_typed_path <- function(path, project = NULL) {
  if (is.null(project) || fs::is_absolute_path(path)) return(path)
  file.path(project, path)
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
        column(6, file_picker(ns, "ai", paste(lab()$ai, "records"),
                              "An ecoextract .db, or a CSV/Excel file.",
                              accept_tables = TRUE)),
        column(6, file_picker(ns, "gold", paste(lab()$gold, "records"),
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
        column(6, file_picker(ns, "ai_papers", paste("Papers processed:", lab()$ai),
                              accept_tables = TRUE)),
        column(6, file_picker(ns, "gold_papers", paste("Papers reviewed:", lab()$gold),
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
    roots <- FILE_ROOTS(isolate(rv$args$project_dir))
    typed <- function(nm) {
      v <- input[[nm]]
      if (is.null(v) || !nzchar(v) || identical(v, FRAME_PLACEHOLDER)) return(v)
      resolve_typed_path(v, rv$args$project_dir)
    }

    # ---- pre-fill from run_eval_app() arguments ---------------------------
    observeEvent(rv$args, {
      for (nm in inputs) {
        v <- rv$args[[nm]]
        if (is.data.frame(v) || inherits(v, "ecoeval_schema")) v <- FRAME_PLACEHOLDER
        if (is.character(v)) updateTextInput(session, nm, value = v)
      }
    }, once = TRUE, ignoreNULL = TRUE)

    # ---- file browsing ----------------------------------------------------
    for (nm in inputs) {
      local({
        this <- nm
        shinyFiles::shinyFileChoose(input, paste0(this, "_browse"),
                                    roots = roots, session = session)
        observeEvent(input[[paste0(this, "_browse")]], {
          sel <- shinyFiles::parseFilePaths(roots,
                                            input[[paste0(this, "_browse")]])
          if (nrow(sel)) updateTextInput(session, this, value = as.character(sel$datapath[[1]]))
        }, ignoreInit = TRUE)

        # A database needs a table chosen; a flat file does not.
        output[[paste0(this, "_table_ui")]] <- renderUI({
          path <- typed(this)
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
      path <- typed("ai")
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
      # A blank box is no input; the placeholder is the object it stands for.
      source_of <- function(nm) {
        v <- input[[nm]]
        if (is.null(v) || !nzchar(v)) return(NULL)
        if (identical(v, FRAME_PLACEHOLDER)) return(rv$args[[nm]])
        typed(nm)
      }
      res <- tryCatch(
        ecoeval::load_inputs(
          ai = source_of("ai"), gold = source_of("gold"),
          schema = source_of("schema"),
          ai_papers = source_of("ai_papers"),
          gold_papers = source_of("gold_papers"),
          ai_table = input$ai_table, gold_table = input$gold_table
        ),
        error = function(e) e
      )

      if (inherits(res, "error")) {
        status(conditionMessage(res))
        showNotification(conditionMessage(res), type = "error", duration = NULL)
        return(invisible(NULL))
      }

      rv$schema <- res$schema
      rv$ai_raw <- res$ai_raw
      rv$gold_raw <- res$gold_raw
      rv$ai_papers_raw <- res$ai_papers_raw
      rv$gold_papers_raw <- res$gold_papers_raw
      rv$documents_raw <- res$documents
      rv$documents <- NULL
      rv$config$inputs[names(res$inputs)] <- res$inputs
      # Loading fresh inputs invalidates everything downstream.
      rv$ai <- NULL; rv$gold <- NULL; rv$scope <- NULL; rv$paper_map <- NULL
      rv$ai_shown <- NULL; rv$gold_shown <- NULL; rv$paper_proposal <- NULL
      rv$comparators <- NULL; rv$field_seed <- NULL
      rv$pairs <- NULL; rv$cells <- NULL
      rv$blocked <- NULL

      status(sprintf("Read %d %s records and %d %s records against %d schema fields.",
                     nrow(res$ai_raw), lab()$ai, nrow(res$gold_raw), lab()$gold,
                     nrow(res$schema$fields)))
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
