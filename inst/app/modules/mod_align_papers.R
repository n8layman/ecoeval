# Stage 3 -- align the papers. This sets scope.
#
# Papers: intersection. Records: union. Scope is the papers present in both
# paper sets -- each source's paper list when supplied, otherwise the papers
# appearing in its records. Within those papers, every record from both sides
# appears; some pair up, some do not.
#
# Papers are a filter, not a scored entity: there is no paper-level confusion
# matrix, and papers outside the intersection are excluded, never penalised.

mod_align_papers_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("summary")),
    eco_panel(
      "Paper links",
      paste("A missed paper match just shrinks the evaluation set. A wrong one",
            "compares one paper's records against another's and produces",
            "garbage -- which is what this review guards against. Untick any",
            "link that is wrong."),
      DT::DTOutput(ns("links")),
      div(style = "margin-top:12px; display:flex; gap:10px; align-items:center;",
          actionButton(ns("accept_all"), "Accept all"),
          actionButton(ns("accept_none"), "Accept none"),
          actionButton(ns("apply"), "Set scope and continue", class = "btn-primary"))
    ),
    uiOutput(ns("excluded"))
  )
}

mod_align_papers_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    accepted <- reactiveVal(NULL)

    proposal <- reactive({
      req(rv$ai, rv$gold)
      ai_p <- rv$ai_papers %||% tibble::tibble(.paper = ecoeval::paper_set(rv$ai))
      gold_p <- rv$gold_papers %||% tibble::tibble(.paper = ecoeval::paper_set(rv$gold))
      ecoeval::align_papers(ai_p, gold_p, rv$metadata_fields %||% character(0))
    })

    observeEvent(proposal(), {
      accepted(proposal()$accepted)
    })

    output$summary <- renderUI({
      p <- proposal()
      n_low <- sum(!p$accepted)
      have_ai <- !is.null(rv$ai_papers)
      have_gold <- !is.null(rv$gold_papers)
      eco_panel(
        "Alignment",
        NULL,
        div(class = "eco-status",
            sprintf("%d of %d links high confidence", sum(p$accepted), nrow(p)),
            if (n_low) sprintf(" · %d need review", n_low)),
        if (!have_ai || !have_gold) {
          div(class = "eco-note", style = "margin-top:8px;",
              if (!have_ai && !have_gold)
                paste("Neither source supplied a paper list, so scope is the",
                      "papers where both sides found something. A paper someone",
                      "read and found nothing in looks identical to one they",
                      "never opened, and drops out. That is self-consistent --",
                      "the exclusion is symmetric.")
              else
                paste("One source supplied a paper list and the other did not,",
                      "so coverage is wider in one direction than the other.",
                      "The warning in the sidebar quantifies it."))
        }
      )
    })

    output$links <- DT::renderDT({
      p <- proposal()
      acc <- accepted() %||% p$accepted
      DT::datatable(
        data.frame(
          Accept = ifelse(acc, "yes", "no"),
          `AI paper` = p$ai_paper,
          `Gold paper` = p$gold_paper,
          Confidence = ifelse(is.na(p$posterior), "--", sprintf("%.3f", p$posterior)),
          Matcher = p$matcher,
          check.names = FALSE
        ),
        rownames = FALSE, selection = "multiple",
        options = list(pageLength = 12, dom = "tip",
                       order = list(list(3, "asc")))
      )
    })

    observeEvent(input$accept_all, accepted(rep(TRUE, nrow(proposal()))))
    observeEvent(input$accept_none, accepted(rep(FALSE, nrow(proposal()))))

    # Clicking a row toggles it, which is quicker than any widget in a table.
    observeEvent(input$links_rows_selected, {
      acc <- accepted()
      if (is.null(acc)) return()
      idx <- input$links_rows_selected
      acc[idx] <- !acc[idx]
      accepted(acc)
      DT::selectRows(DT::dataTableProxy("links"), NULL)
    }, ignoreNULL = TRUE)

    observeEvent(input$apply, {
      p <- proposal()
      acc <- accepted() %||% p$accepted
      pm <- p[acc, c("ai_paper", "gold_paper", "posterior", "matcher"), drop = FALSE]
      pm$accepted <- TRUE
      rv$paper_map <- pm

      ai_set <- ecoeval::paper_set(rv$ai, rv$ai_papers)
      gold_set <- ecoeval::paper_set(rv$gold, rv$gold_papers)
      full_scope <- ecoeval::compute_scope(ai_set, gold_set)
      # Scope is what the human accepted, not everything the matcher proposed.
      scope <- list(
        papers = intersect(full_scope$papers, pm$ai_paper),
        ai_only = union(full_scope$ai_only, setdiff(full_scope$papers, pm$ai_paper)),
        gold_only = union(full_scope$gold_only,
                          setdiff(full_scope$papers, pm$gold_paper))
      )
      rv$scope <- scope

      warn <- ecoeval::coverage_warning(!is.null(rv$ai_papers),
                                        !is.null(rv$gold_papers), scope)
      rv$warnings <- unique(c(rv$warnings, warn))

      # Zero paper overlap is one of the only two conditions that stop the app,
      # because it leaves nothing to compare.
      if (!length(scope$papers)) {
        rv$blocked <- ecoeval::blocking_condition(scope, names(rv$ai))
        return(invisible(NULL))
      }
      rv$blocked <- NULL
      rv$pairs <- NULL; rv$cells <- NULL
      rv$stage <- "fields"
    })

    output$excluded <- renderUI({
      if (is.null(rv$scope)) return(NULL)
      show <- function(x, label, why) {
        if (!length(x)) return(NULL)
        tags$details(
          tags$summary(sprintf("%d %s -- %s", length(x), label, why)),
          tags$div(style = "font-size:12px; color:#6b7280; margin-top:6px;",
                   paste(utils::head(x, 40), collapse = ", "),
                   if (length(x) > 40) sprintf(" ... and %d more", length(x) - 40))
        )
      }
      eco_panel(
        "Excluded from scope", 
        "Excluded, never penalised. They simply narrow the comparison set.",
        show(rv$scope$ai_only, "AI-only papers",
             "the gold standard has nothing for them"),
        show(rv$scope$gold_only, "gold-only papers",
             "the AI has nothing for them")
      )
    })
  })
}
