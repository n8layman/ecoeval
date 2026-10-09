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
    ai          = list(key = "ai", label = "AI records", df = rv$ai_raw),
    gold        = list(key = "gold", label = "Gold standard records",
                       df = rv$gold_raw),
    ai_papers   = list(key = "ai_papers", label = "AI paper list",
                       df = rv$ai_papers_raw),
    gold_papers = list(key = "gold_papers", label = "Gold paper list",
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

    # What a table would be keyed on under the chosen strategy, falling back to
    # its own best when it cannot supply that one.
    auto_key <- function(tk) {
      cand <- candidates()[[tk]]
      if (!length(cand)) return(NULL)
      s <- strategy()
      if (!is.na(s) && !is.null(cand[[s]])) cand[[s]] else cand[[1L]]
    }

    # The manual override wins when it is set; it is pre-filled with auto_key(),
    # so in the normal case the two are the same thing.
    chosen_key <- function(tk) {
      manual <- input[[paste0(tk, "_cols")]]
      if (length(manual)) ecoeval::paper_key(manual) else auto_key(tk)
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
          k <- auto_key(t$key)
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
      res <- tryCatch({
        rv$ai_paper_key <- chosen_key("ai")
        rv$gold_paper_key <- chosen_key("gold")

        # Records keep every column at this stage; the record-field mapping in
        # stage 4 decides which of them are actually compared.
        rv$ai <- ecoeval::prepare_records(rv$ai_raw, rv$ai_paper_key, prefix = "a")
        rv$gold <- ecoeval::prepare_records(rv$gold_raw, rv$gold_paper_key,
                                            prefix = "g")

        # Metadata for paper alignment comes from the same role detection that
        # found the key -- title, author, year, whatever the list carries.
        ai_meta <- if (!is.null(rv$ai_papers_raw)) {
          ecoeval::paper_metadata_columns(rv$ai_papers_raw)
        } else character(0)
        gold_meta <- if (!is.null(rv$gold_papers_raw)) {
          ecoeval::paper_metadata_columns(rv$gold_papers_raw)
        } else character(0)

        rv$ai_papers <- if (!is.null(rv$ai_papers_raw)) {
          rv$ai_papers_key <- chosen_key("ai_papers")
          ecoeval::prepare_papers(rv$ai_papers_raw, rv$ai_papers_key, ai_meta)
        }
        rv$gold_papers <- if (!is.null(rv$gold_papers_raw)) {
          rv$gold_papers_key <- chosen_key("gold_papers")
          ecoeval::prepare_papers(rv$gold_papers_raw, rv$gold_papers_key, gold_meta)
        }
        rv$metadata_fields <- intersect(names(ai_meta), names(gold_meta))
        TRUE
      }, error = function(e) e)

      if (inherits(res, "error")) {
        status(conditionMessage(res))
        showNotification(conditionMessage(res), type = "error", duration = NULL)
        return(invisible(NULL))
      }

      key_cols <- function(k) if (is.null(k)) NULL else k$columns
      rv$config$inputs$ai_paper_key <- key_cols(rv$ai_paper_key)
      rv$config$inputs$gold_paper_key <- key_cols(rv$gold_paper_key)
      rv$config$inputs$ai_papers_paper_key <- key_cols(rv$ai_papers_key)
      rv$config$inputs$gold_papers_paper_key <- key_cols(rv$gold_papers_key)

      # Internal consistency, run at load: every paper referenced in a record
      # list should appear in that source's paper list.
      checks <- dplyr::bind_rows(
        ecoeval::source_consistency(rv$ai, rv$ai_papers, "ai"),
        ecoeval::source_consistency(rv$gold, rv$gold_papers, "gold")
      )
      unplaced <- sum(is.na(rv$ai$.paper)) + sum(is.na(rv$gold$.paper))
      # Keys of different kinds cannot match, so scope would come out empty --
      # say so here rather than let the next stage report nothing to compare.
      mismatched <- !is.null(rv$ai_paper_key) && !is.null(rv$gold_paper_key) &&
        !identical(rv$ai_paper_key$strategy, rv$gold_paper_key$strategy)
      rv$warnings <- unique(c(
        setdiff(rv$warnings, checks$detail),
        if (nrow(checks)) paste0(toupper(checks$source), ": ", checks$detail),
        if (unplaced) sprintf(
          paste("%d records carry no paper identifier and cannot be compared.",
                "Check the identifier column."), unplaced),
        if (mismatched) sprintf(
          paste("The two sources are identified differently -- AI by %s, gold",
                "by %s -- so their keys cannot line up. Pick columns of the",
                "same kind on both sides."),
          format(rv$ai_paper_key), format(rv$gold_paper_key))
      ))
      status(sprintf("%d AI records and %d gold records placed.",
                     sum(!is.na(rv$ai$.paper)), sum(!is.na(rv$gold$.paper))))
      rv$stage <- "papers"
    })
  })
}
