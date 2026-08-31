# Stage 6 -- the dashboard.
#
# Two ways to use this app, and neither is the shortcut. "How accurate is it?"
# presses Resolve all differences, reads the numbers, and never opens a single
# paper -- this is the common case and should feel like the main road. "Why is
# it wrong?" walks the papers looking for the patterns worth fixing before the
# next extraction run.
#
# Two confusion matrices, both about extraction. There is no third matrix about
# linkage quality: users should not have to learn that distinction.

mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("headline")),
    uiOutput(ns("resolve_panel")),
    eco_panel(
      "Column accuracy, worst first",
      paste("The triage view -- but read it carefully. A column that is",
            "uniformly wrong has three possible causes and this chart cannot",
            "tell them apart: a misconfigured comparator, an ambiguous field",
            "description, or the model genuinely failing. Check them in that",
            "order."),
      plotOutput(ns("column_accuracy"), height = "auto"),
      div(style = "margin-top:8px;",
          downloadButton(ns("dl_column_accuracy"), "Download PNG", class = "btn-sm"))
    ),
    eco_panel(
      "Per-column detail",
      paste("The form adapts to the column: a true K-by-K for an enum, a",
            "presence-absence split into correct and wrong value for free",
            "text, and an error distribution for numbers."),
      selectInput(ns("column"), NULL, choices = NULL, width = "340px"),
      uiOutput(ns("column_notes")),
      plotly::plotlyOutput(ns("confusion_interactive"), height = "380px"),
      plotOutput(ns("errors"), height = "230px"),
      div(style = "margin-top:8px;",
          downloadButton(ns("dl_confusion"), "Download PNG", class = "btn-sm"))
    ),
    eco_panel(
      "Records and completeness", NULL,
      fluidRow(
        column(6, plotOutput(ns("record_outcome"), height = "230px")),
        column(6, plotOutput(ns("completeness"), height = "auto"))
      )
    ),
    uiOutput(ns("findings")),
    eco_panel(
      "Compare with a previous run",
      paste("Without run-over-run comparison, iterating is blind. Load the",
            "run_config.json from an earlier evaluation to see which numbers",
            "moved after a schema or prompt change."),
      fileInput(ns("previous"), NULL, accept = ".json", width = "420px"),
      uiOutput(ns("diff"))
    ),
    eco_panel(
      "Export",
      paste("The session artifact and the export bundle are the same object,",
            "so save, restore, and download share one implementation. Reload",
            "run_config.json and you are exactly where you were -- manual link",
            "decisions and cached judge verdicts included."),
      div(style = "display:flex; gap:10px; flex-wrap:wrap; align-items:center;",
          downloadButton(ns("dl_bundle"), "Download the bundle (.zip)",
                         class = "btn-primary"),
          downloadButton(ns("dl_config"), "Save run_config.json"),
          div(class = "eco-status", textOutput(ns("export_status"), inline = TRUE)))
    )
  )
}

mod_dashboard_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    fm <- reactive({ req(rv$cells); ecoeval::field_metrics(rv$cells) })
    agg <- reactive({ req(rv$cells); ecoeval::aggregate_metrics(rv$cells) })
    rmet <- reactive({ req(rv$pairs); ecoeval::record_metrics(rv$pairs) })

    # ---- the headline numbers ----------------------------------------------
    output$headline <- renderUI({
      req(rv$cells)
      a <- agg(); r <- rmet()
      prog <- ecoeval::progress_summary(rv$cells, rv$scope$papers, rv$reviewed)
      tagList(
        div(class = "eco-figures",
            figure_tile(fmt_pct(a$overall_accuracy), "Overall accuracy",
                        sprintf("%d scored cells", a$n_scored)),
            figure_tile(fmt_pct(a$column_mean_accuracy), "Average across columns",
                        sprintf("%d columns, weighted equally", a$n_columns)),
            figure_tile(fmt_pct(r$precision), "Record precision",
                        sprintf("%d matched, %d AI-only", r$tp, r$fp)),
            figure_tile(fmt_pct(r$recall), "Record recall",
                        sprintf("%d matched, %d gold-only", r$tp, r$fn)),
            figure_tile(fmt_pct(r$f1), "Record F1", NULL)),
        div(class = "eco-panel", style = "margin-top:14px;",
            div(class = "eco-status",
                sprintf("%d of %d papers judged · %d reviewed",
                        prog$n_judged, prog$n_papers, prog$n_reviewed),
                if (!is.null(ecoeval::correction_count(rv$rejected, rv$added,
                                                       rv$overrides)))
                  span(" · ", ecoeval::correction_count(rv$rejected, rv$added,
                                                        rv$overrides))),
            div(class = "eco-note", style = "margin-top:6px;",
                paste("Field-level numbers are largely robust to alignment",
                      "error -- a wrongly paired row that disagrees everywhere",
                      "scores the same as two unpaired rows. Record-level",
                      "precision and recall are not, since pairing converts one",
                      "false positive and one false negative into one true",
                      "positive.")))
      )
    })

    # ---- resolve all differences -------------------------------------------
    output$resolve_panel <- renderUI({
      req(rv$cells)
      n <- ecoeval::estimate_judge_calls(rv$cells)
      susp <- ecoeval::suspect_pairs(rv$cells, identity_fields(rv))
      tagList(
        if (n > 0L) eco_panel(
          "Resolve all differences",
          paste("The judge runs across every scoped paper on paired rows only,",
                "and only on cells the cheaper rungs could not settle.",
                "Verdicts are cached as they complete, so an interrupted run",
                "picks up where it stopped, and a reproducible run has them",
                "frozen rather than re-derived."),
          div(class = "eco-status",
              sprintf("%d cells are unjudged. That is about %d LLM calls.", 
                      sum(rv$cells$pending), n)),
          if (!ecoeval::judge_available())
            div(class = "eco-warn", style = "margin-top:8px;",
                "No LLM is configured. Set ANTHROPIC_API_KEY in .env and",
                "install ellmer to enable the judge. Until then those cells",
                "count as disagreements and the dashboard says so.")
          else div(style = "margin-top:10px;",
                   actionButton(ns("resolve"), sprintf("Run %d calls", n),
                                class = "btn-primary"))
        ),
        if (nrow(susp)) eco_panel(
          "Look at these first",
          paste("The batch pass knows which pairs disagree on everything, and",
                "which disagree on every identity column. That is the bridge",
                "between reading the numbers and reviewing by hand: spend the",
                "effort only where it is worth spending."),
          tags$pre(class = "eco-status",
                   paste(sprintf("  %-28s %s", susp$paper, susp$reason),
                         collapse = "\n")),
          actionButton(ns("go_review"), "Review these papers")
        )
      )
    })

    observeEvent(input$go_review, {
      susp <- ecoeval::suspect_pairs(rv$cells, identity_fields(rv))
      if (nrow(susp)) rv$current_paper <- susp$paper[[1L]]
      rv$stage <- "compare"
    })

    observeEvent(input$resolve, {
      judge <- ecoeval::make_judge(
        descriptions = stats::setNames(rv$schema$fields$description,
                                       rv$schema$fields$field)
      )
      if (is.null(judge)) {
        showNotification("No LLM is available.", type = "error")
        return()
      }
      use <- rv$comparators[rv$comparators$include, , drop = FALSE]
      withProgress(message = "Asking the judge", value = 0, {
        rv$cells <- ecoeval::score_cells(
          rv$pairs, rv$ai, rv$gold, use,
          judge = judge, cache = rv$judge_cache, overrides = rv$overrides,
          progress = function(i, n) setProgress(i / n, detail = sprintf("%d of %d", i, n))
        )
      })
      rv$dirty <- rv$dirty + 1L
      showNotification("The judge has settled the contested cells.", type = "message")
    })

    # ---- charts -------------------------------------------------------------
    output$column_accuracy <- renderPlot(
      ecoeval::plot_column_accuracy(fm()),
      height = function() max(240, 34 * nrow(fm()) + 90)
    )
    output$record_outcome <- renderPlot(ecoeval::plot_record_outcome(rmet()))
    output$completeness <- renderPlot(
      ecoeval::plot_completeness(ecoeval::fill_rates(rv$cells)),
      height = function() max(240, 34 * nrow(fm()) + 90)
    )

    observeEvent(fm(), {
      updateSelectInput(session, "column", choices = fm()$field,
                        selected = input$column %||% fm()$field[[1L]])
    })

    cc <- reactive({
      req(input$column, rv$cells)
      ecoeval::column_confusion(rv$cells, input$column, rv$schema)
    })

    output$column_notes <- renderUI({
      notes <- cc()$notes
      if (!length(notes)) return(NULL)
      tagList(lapply(notes, function(n) div(class = "eco-warn", n)))
    })
    # Hovering a tile to read its count is worth the dependency; if plotly
    # cannot convert the plot, fall back to the static one rather than nothing.
    output$confusion_interactive <- plotly::renderPlotly({
      p <- ecoeval::plot_confusion(cc())
      tryCatch(plotly::ggplotly(p), error = function(e) plotly::ggplotly(ggplot2::ggplot()))
    })
    output$confusion <- renderPlot(ecoeval::plot_confusion(cc()))
    output$errors <- renderPlot({
      d <- cc()
      if (is.null(d$errors) || !nrow(d$errors)) return(NULL)
      ecoeval::plot_error_distribution(d$errors, d$field)
    })

    png_download <- function(plot_fn, name, height = 5) {
      downloadHandler(
        filename = function() paste0(name, ".png"),
        content = function(file) {
          ggplot2::ggsave(file, plot_fn(), width = 8, height = height, dpi = 150,
                          bg = "white")
        }
      )
    }
    output$dl_column_accuracy <- png_download(
      function() ecoeval::plot_column_accuracy(fm()), "column_accuracy",
      height = max(3, 0.4 * nrow(fm()) + 1.5))
    output$dl_confusion <- png_download(
      function() ecoeval::plot_confusion(cc()),
      paste0("confusion_", input$column %||% "column"))

    # ---- findings -----------------------------------------------------------
    findings <- reactive({
      req(rv$cells, rv$pairs)
      ecoeval::collect_findings(
        rv$cells, rv$pairs, rv$conformance, rv$schema,
        gold_fields = setdiff(names(rv$gold_raw), rv$gold_paper_col),
        collapses = rv$collapses,
        dropped_fields = dropped_fields(rv),
        linkage_fields = identity_fields(rv)
      )
    })

    output$findings <- renderUI({
      f <- findings()
      if (!nrow(f)) return(NULL)
      titles <- c(schema = "Schema and prompt", model = "Model",
                  gold = "Gold standard")
      notes <- c(
        schema = "Things to change before the next extraction run.",
        model = "How the model actually did.",
        gold = "Signs the reference data needs cleaning rather than the schema needs changing."
      )
      eco_panel(
        "Findings",
        paste("Output, not gates. Grouped by who acts on them. Nothing here",
              "adjusts a metric -- the groups exist so you know where to go",
              "next."),
        lapply(names(titles), function(g) {
          d <- f[f$group == g, , drop = FALSE]
          if (!nrow(d)) return(NULL)
          tagList(
            tags$h4(titles[[g]], style = "font-size:13.5px; margin-top:14px;"),
            div(class = "eco-note", notes[[g]]),
            tags$ul(style = "font-size:12.8px; padding-left:18px;",
                    lapply(seq_len(nrow(d)), function(i) {
                      tags$li(tags$strong(d$finding[[i]]), " — ", d$detail[[i]])
                    }))
          )
        })
      )
    })

    # ---- run-over-run diff --------------------------------------------------
    output$diff <- renderUI({
      f <- input$previous
      if (is.null(f)) return(NULL)
      before <- tryCatch(ecoeval::read_run_config(f$datapath),
                         error = function(e) e)
      if (inherits(before, "error")) {
        return(div(class = "eco-warn",
                   "Could not read that file: ", conditionMessage(before)))
      }
      d <- ecoeval::diff_runs(before, current_config())
      if (!nrow(d) || all(is.na(d$delta))) {
        return(div(class = "eco-status",
                   "That run recorded no metrics to compare against."))
      }
      label <- c(overall_accuracy = "Overall accuracy",
                 column_mean_accuracy = "Average across columns",
                 record_precision = "Record precision",
                 record_recall = "Record recall",
                 record_f1 = "Record F1")
      tags$ul(
        style = "font-size:13px; padding-left:18px;",
        lapply(seq_len(nrow(d)), function(i) {
          if (is.na(d$delta[[i]])) return(NULL)
          arrow <- if (d$delta[[i]] > 0) "\u2191" else if (d$delta[[i]] < 0) "\u2193" else "\u2192"
          colour <- if (d$delta[[i]] > 0) "#2e9e5b" else if (d$delta[[i]] < 0) "#d9534f" else "#6b7280"
          tags$li(
            tags$strong(label[[d$metric[[i]]]] %||% d$metric[[i]]), " ",
            fmt_pct(d$before[[i]]), " \u2192 ", fmt_pct(d$after[[i]]),
            span(style = sprintf("color:%s; margin-left:6px;", colour),
                 sprintf("%s %s", arrow, fmt_pct(abs(d$delta[[i]]))))
          )
        })
      )
    })

    # ---- export -------------------------------------------------------------
    current_config <- reactive({
      ecoeval::capture_run_config(
        rv$config,
        comparators = rv$comparators, paper_map = rv$paper_map,
        rejected = rv$rejected, added = rv$added, overrides = rv$overrides,
        cache = rv$judge_cache, norm_cache = rv$norm_cache,
        reviewed = rv$reviewed, scope = rv$scope,
        metrics = list(
          overall_accuracy = agg()$overall_accuracy,
          column_mean_accuracy = agg()$column_mean_accuracy,
          record_precision = rmet()$precision,
          record_recall = rmet()$recall,
          record_f1 = rmet()$f1
        )
      )
    })

    output$export_status <- renderText({
      req(rv$cells)
      sprintf("%d rows, %d columns scored.", nrow(rv$pairs), nrow(fm()))
    })

    output$dl_config <- downloadHandler(
      filename = function() "run_config.json",
      content = function(file) ecoeval::write_run_config(current_config(), file)
    )

    output$dl_bundle <- downloadHandler(
      filename = function() {
        paste0("ecoeval_run_", format(Sys.Date(), "%Y-%m-%d"), ".zip")
      },
      content = function(file) {
        tmp <- file.path(tempdir(), paste0("ecoeval_run_",
                                           format(Sys.Date(), "%Y-%m-%d")))
        unlink(tmp, recursive = TRUE)
        withProgress(message = "Building the bundle", value = 0.3, {
          ecoeval::export_bundle(
            tmp, rv$cells, rv$pairs, current_config(),
            findings = findings(), scope = rv$scope, schema = rv$schema,
            patch = ecoeval::schema_patch(rv$conformance, rv$collapses,
                                          rv$cells, rv$schema),
            warnings = rv$warnings,
            progress = ecoeval::progress_summary(rv$cells, rv$scope$papers,
                                                 rv$reviewed),
            corrections = ecoeval::correction_count(rv$rejected, rv$added,
                                                    rv$overrides)
          )
          incProgress(0.6, detail = "zipping")
          zip_path <- ecoeval::zip_bundle(tmp)
          file.copy(zip_path, file, overwrite = TRUE)
        })
      }
    )
  })
}

#' Columns that exist on only one side, so there is nothing to compare
#'
#' Excluded from the grid entirely, but reported as a finding so the omission
#' is visible.
dropped_fields <- function(rv) {
  if (is.null(rv$comparators)) return(character(0))
  rv$comparators$field[!rv$comparators$include]
}
