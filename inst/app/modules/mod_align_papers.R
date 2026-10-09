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

    # The matcher's proposal -- reused when it already ran at launch -- plus
    # any link in force it did not propose, such as one supplied as paper_map.
    proposal <- reactive({
      req(rv$ai, rv$gold)
      p <- rv$paper_proposal %||%
        ecoeval::propose_paper_links(list(ai = rv$ai, gold = rv$gold,
                                          ai_papers = rv$ai_papers,
                                          gold_papers = rv$gold_papers,
                                          metadata_fields = rv$metadata_fields))
      pm <- isolate(rv$paper_map)
      if (!is.null(pm) && nrow(pm)) {
        extra <- pm[!paste(pm$ai_paper, pm$gold_paper) %in%
                      paste(p$ai_paper, p$gold_paper), , drop = FALSE]
        if (nrow(extra)) {
          p <- dplyr::bind_rows(p, tibble::tibble(
            ai_paper = extra$ai_paper, gold_paper = extra$gold_paper,
            posterior = NA_real_, matcher = "supplied", accepted = TRUE))
        }
      }
      p
    })

    # Links already in force start ticked; otherwise the matcher's verdict.
    observeEvent(proposal(), {
      p <- proposal()
      pm <- isolate(rv$paper_map)
      accepted(if (!is.null(pm) && nrow(pm)) {
        paste(p$ai_paper, p$gold_paper) %in% paste(pm$ai_paper, pm$gold_paper)
      } else p$accepted)
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
      res <- ecoeval::set_scope(
        list(ai = rv$ai, gold = rv$gold, ai_papers = rv$ai_papers,
             gold_papers = rv$gold_papers),
        p[acc, , drop = FALSE]
      )
      rv$paper_map <- res$paper_map
      rv$scope <- res$scope
      # The links were just reviewed, so the launch-time note about the ones
      # left out unreviewed no longer applies.
      rv$warnings <- unique(c(
        rv$warnings[!grepl("fell below the confidence cutoff", rv$warnings)],
        res$warnings))

      # Zero paper overlap is one of the only two conditions that stop the app,
      # because it leaves nothing to compare.
      if (!is.null(res$blocked)) {
        rv$blocked <- res$blocked
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
