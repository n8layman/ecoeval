# Stage 2 -- identify the papers.
#
# This has to precede paper alignment: before we can decide which AI paper is
# which gold paper, we need to know what names a paper. There is nothing to
# decide by hand here in the normal case -- ecoeval works down a fixed priority
# list (DOI, file name, title, first author + year) over every table it was
# given and shows what it found. The list is applied to all tables at once, so
# the two sources produce keys of the same kind and are actually comparable.
#
# Paper alignment determines scope, not score -- but its asymmetry is worth
# knowing. A missed paper match just shrinks the evaluation set; a wrong paper
# match compares one paper's records against another's and produces garbage.

# The tables that need a paper key, in the order they are shown.
paper_tables <- function(rv) {
  defs <- list(
    ai          = list(key = "ai", label = paste(lab()$ai, "records"), df = rv$ai_raw),
    gold        = list(key = "gold", label = paste(lab()$gold, "records"),
                       df = rv$gold_raw),
    ai_papers   = list(key = "ai_papers", label = paste(lab()$ai, "paper list"),
                       df = rv$ai_papers_raw),
    gold_papers = list(key = "gold_papers", label = paste(lab()$gold, "paper list"),
                       df = rv$gold_papers_raw)
  )
  Filter(function(d) !is.null(d$df), defs)
}

mod_map_metadata_ui <- function(id) {
  ns <- NS(id)
  tagList(
    eco_panel(
      "Which column identifies the paper?",
      paste("Detected automatically, in priority order: DOI -- the only",
            "identifier in this domain that is actually unique -- then file",
            "name, then title, then first author and year. An identifier can",
            "span more than one column; author and year is why."),
      uiOutput(ns("detected"))
    ),
    uiOutput(ns("override")),
    div(class = "eco-panel",
        div(style = "display:flex; gap:10px; align-items:center;",
            actionButton(ns("apply"), "Apply and continue", class = "btn-primary"),
            div(class = "eco-status", textOutput(ns("status"), inline = TRUE))))
  )
}

mod_map_metadata_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tables <- reactive({
      req(rv$ai_raw, rv$gold_raw)
      paper_tables(rv)
    })

    # Every identifier each table could be keyed on, best first.
    candidates <- reactive({
      lapply(tables(), function(t) ecoeval::paper_key_candidates(t$df))
    })

    # The identifiers every table can supply. Keying both sources the same way
    # is what makes the keys comparable, so a strategy one side cannot produce
    # is not offered.
    shared <- reactive({
      Reduce(intersect, lapply(candidates(), names)) %||% character(0)
    })

    strategy <- reactive({
      s <- input$strategy
      if (!is.null(s) && nzchar(s)) return(s)
      sh <- shared()
      if (length(sh)) sh[[1L]] else NA_character_
    })

    # What every table would be keyed on under the chosen strategy, each
    # falling back to its own best when it cannot supply that one.
    auto_keys <- reactive({
      ecoeval::choose_paper_keys(lapply(tables(), `[[`, "df"),
                                 strategy = strategy())
    })
    auto_key <- function(tk) auto_keys()[[tk]]

    # The manual override wins when it is set; it is pre-filled with what is
    # already in force -- a key supplied at launch, else auto_key() -- so in
    # the normal case the two are the same thing.
    chosen_key <- function(tk) {
      manual <- input[[paste0(tk, "_cols")]]
      if (length(manual)) ecoeval::paper_key(manual) else auto_key(tk)
    }
    current_key <- function(tk) {
      rv[[switch(tk, ai = "ai_paper_key", gold = "gold_paper_key",
                 ai_papers = "ai_papers_key", gold_papers = "gold_papers_key")]]
    }

    output$detected <- renderUI({
      cand <- candidates()
      sh <- shared()
      labels <- vapply(sh, function(s) {
        cand[[1L]][[s]]$label
      }, character(1))
      rows <- lapply(tables(), function(t) {
        k <- chosen_key(t$key)
        tags$li(tags$strong(t$label), ": ",
                if (is.null(k)) tags$span(class = "eco-warn-inline",
                                          "nothing identifies a paper")
                else format(k))
      })
      tagList(
        if (length(sh) > 1L) {
          radioButtons(ns("strategy"), NULL, inline = TRUE,
                       choices = stats::setNames(sh, labels),
                       selected = isolate(strategy()))
        },
        tags$ul(style = "font-size:12.5px; padding-left:18px; margin:6px 0 0;",
                rows),
        if (!length(sh)) {
          div(class = "eco-note",
              "No single identifier works across every table, so each is keyed",
              "on its own best column. If the two sides do not line up, pick",
              "the columns by hand below.")
        }
      )
    })

    output$override <- renderUI({
      tags$details(
        class = "eco-panel",
        tags$summary("Pick the columns by hand"),
        div(class = "eco-note",
            "Only needed when the detected column is wrong. Several columns",
            "make a compound key; they are joined in the order given."),
        fluidRow(lapply(tables(), function(t) {
          k <- isolate(current_key(t$key)) %||% auto_key(t$key)
          column(6, selectInput(
            ns(paste0(t$key, "_cols")), t$label,
            choices = names(t$df), multiple = TRUE, width = "100%",
            selected = if (is.null(k)) NULL else k$columns))
        }))
      )
    })

    status <- reactiveVal("")
    output$status <- renderText(status())

    observeEvent(input$apply, {
      req(rv$ai_raw, rv$gold_raw)
      keys <- lapply(stats::setNames(nm = names(tables())), chosen_key)
      res <- tryCatch(ecoeval::place_papers(loaded_inputs(rv), keys),
                      error = function(e) e)

      if (inherits(res, "error")) {
        status(conditionMessage(res))
        showNotification(conditionMessage(res), type = "error", duration = NULL)
        return(invisible(NULL))
      }

      rv$ai_paper_key <- keys$ai
      rv$gold_paper_key <- keys$gold
      rv$ai_papers_key <- keys$ai_papers
      rv$gold_papers_key <- keys$gold_papers
      record_paper_keys(rv)
      rv$ai <- res$ai
      rv$gold <- res$gold
      rv$ai_papers <- res$ai_papers
      rv$gold_papers <- res$gold_papers
      rv$metadata_fields <- res$metadata_fields
      rv$documents <- res$documents
      # New keys mean new paper identifiers, so links made under the old ones
      # no longer apply.
      rv$paper_proposal <- NULL
      rv$paper_map <- NULL
      rv$scope <- NULL
      rv$warnings <- unique(c(rv$warnings, res$warnings))
      status(sprintf("%d %s records and %d %s records placed.",
                     sum(!is.na(rv$ai$.paper)), lab()$ai,
                     sum(!is.na(rv$gold$.paper)), lab()$gold))
      rv$stage <- "papers"
    })
  })
}
