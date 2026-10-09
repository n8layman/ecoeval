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

# Past this many papers the overview heatmap is a wall of pixels rather than a
# chart, so it shows the worst ones and says how many it left out.
HEATMAP_MAX_PAPERS <- 60L

# Names the plotly click events come back under.
HEATMAP_SOURCE <- "eco_heatmap"

# ggplotly gives every tile its own trace to make it hoverable, which stops
# being worth it on a big grid: past this many tiles the heatmap renders as a
# static plot instead of a slow interactive one.
HEATMAP_MAX_TILES <- 900L

# The confusion matrix as its four boxes, coloured like the tiles they count.
# The populated-by-both box splits into agree and differ, which is the split
# that separates "did not fill the field in" from "filled it in wrong".
confusion_matrix_ui <- function(n, labels = lab()) {
  only <- function(side) paste(labels[[side]], "only")
  cost <- function(cm, neutral) if (labels$neutral) neutral else cm
  box <- function(colour, label, count, contributes) {
    div(class = paste0("eco-cm-box eco-", colour),
        div(class = "eco-cm-n", format(count, big.mark = ",")),
        div(class = "eco-cm-label", label),
        div(class = "eco-cm-role", contributes))
  }
  head_cell <- function(...) tags$th(class = "eco-cm-head", ...)
  tags$table(
    class = "eco-cm",
    tags$thead(tags$tr(
      tags$th(""),
      head_cell(paste(labels$gold, "has a value")),
      head_cell(paste(labels$gold, "is blank"))
    )),
    tags$tbody(
      tags$tr(
        head_cell(paste(labels$ai, "has a value")),
        tags$td(
          box("green", "They agree", n[["agree"]],
              cost("true positive", "agreement")),
          box("purple", "They differ", n[["disagree"]],
              cost("false positive + false negative", "a difference"))
        ),
        tags$td(box("orange", only("ai"), n[["only_ai"]],
                    cost("false positive", paste("missing from", labels$gold))))
      ),
      tags$tr(
        head_cell(paste(labels$ai, "is blank")),
        tags$td(box("yellow", only("gold"), n[["only_gold"]],
                    cost("false negative", paste("missing from", labels$ai)))),
        tags$td(box("green", "Neither side", n[["blank"]],
                    cost("true negative -- drops out", "drops out")))
      )
    )
  )
}

mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("headline")),
    uiOutput(ns("resolve_panel")),
    eco_panel(
      "Every paper, every column",
      paste("The whole run in one picture. Rows are papers, columns are the",
            "gold standard's columns, and a tile is that paper's records for",
            "that column, shaded by how much of it agrees -- dark where the",
            "two sides match on everything, pale where they match on little.",
            "It shades a rate rather than one of the four outcome colours",
            "because a tile covers several records, and those colours describe",
            "a single cell; they are exact one paper at a time, which is what",
            "clicking a tile opens. Both axes are sorted worst first: a pale",
            "vertical band is a column that fails everywhere, usually a",
            "comparator or schema problem; a pale horizontal one is a paper",
            "that fails everywhere, usually a bad alignment. Hover a tile for",
            "what is behind it, counted in rows of that paper's comparison --",
            "a matched pair is one row, an unmatched record is one row. Click",
            "a column name to narrow everything below to that column."),
      uiOutput(ns("heatmap_ui")),
      uiOutput(ns("confusion_matrix")),
      div(style = "margin-top:8px;",
          downloadButton(ns("dl_heatmap"), "Download PNG", class = "btn-sm"))
    ),
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
                        sprintf("%d matched, %d %s only", r$tp, r$fp, lab()$ai)),
            figure_tile(fmt_pct(r$recall), "Record recall",
                        sprintf("%d matched, %d %s only", r$tp, r$fn, lab()$gold)),
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
          if (identical(rv$judge_mode, "off"))
            div(class = "eco-note", style = "margin-top:8px;",
                "The LLM is switched off for this run (judge = NULL), so these",
                "cells stay unjudged and count as disagreements.")
          else if (identical(rv$judge_mode, "default") && !ecoeval::judge_available())
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
      judge <- current_judge(rv)
      if (is.null(judge)) {
        showNotification("No LLM is available.", type = "error")
        return()
      }
      withProgress(message = "Asking the judge", value = 0, {
        rescore(rv, judge = judge, progress = function(i, n) {
          setProgress(i / n, detail = sprintf("%d of %d", i, n))
        })
      })
      showNotification("The judge has settled the contested cells.", type = "message")
    })

    # ---- charts -------------------------------------------------------------

    # The overview heatmap. Every scoped paper gets a row, including one that
    # produced nothing, since an empty row is itself a finding.
    heat <- reactive({
      req(rv$cells)
      ecoeval::paper_field_outcomes(rv$cells, rv$scope$papers, scored_fields(rv))
    })
    heat_papers <- reactive(min(length(unique(heat()$paper)), HEATMAP_MAX_PAPERS))
    heat_plot <- function() {
      ecoeval::plot_paper_heatmap(heat(), max_papers = HEATMAP_MAX_PAPERS)
    }

    output$heatmap_ui <- renderUI({
      h <- sprintf("%dpx", 26 * heat_papers() + 230)
      n_tiles <- heat_papers() * length(unique(heat()$field))
      if (n_tiles <= HEATMAP_MAX_TILES) {
        plotly::plotlyOutput(ns("heatmap"), height = h)
      } else {
        tagList(
          plotOutput(ns("heatmap_static"), height = h, click = ns("heatmap_click")),
          div(class = "eco-note", style = "margin-top:6px;",
              sprintf(paste("%d tiles is too many to make every one hoverable,",
                            "so this one is static. The counts behind a tile",
                            "are in the exported table."), n_tiles))
        )
      }
    })
    output$heatmap <- plotly::renderPlotly({
      interactive_heatmap(heat_plot(), HEATMAP_SOURCE, ns("column_click"))
    })
    output$heatmap_static <- renderPlot(heat_plot())

    # ---- the same colours, counted ------------------------------------------
    # Which column the matrix is scoped to; NULL means all of them.
    matrix_field <- reactiveVal(NULL)

    # Reset when the run changes underneath it, so a stale column cannot linger.
    observeEvent(rv$cells, {
      f <- matrix_field()
      if (!is.null(f) && !f %in% rv$cells$field) matrix_field(NULL)
    })

    # The heatmap and the headline numbers are one thing seen two ways, so the
    # arithmetic is written out rather than asserted: these are the tiles'
    # colours added up, and the metrics fall straight out of them.
    output$confusion_matrix <- renderUI({
      req(rv$cells)
      field <- matrix_field()
      ct <- ecoeval::confusion_totals(rv$cells, field)
      n <- stats::setNames(ct$by_outcome$n, ct$by_outcome$outcome)

      ratio <- function(label, num, den, value) {
        span(style = "margin-right:18px;",
             tags$strong(label), " ",
             sprintf("%d / %d = %s", num, den, fmt_pct(value)))
      }

      div(
        style = "margin-top:16px;",
        div(style = "display:flex; align-items:baseline; gap:10px; flex-wrap:wrap;",
            tags$h4(style = "font-size:13.5px; margin:0;",
                    if (is.null(field)) "Confusion matrix \u2014 every column"
                    else paste("Confusion matrix \u2014", field)),
            if (!is.null(field))
              actionLink(ns("matrix_all"), "show every column",
                         class = "eco-status")),
        confusion_matrix_ui(n),
        div(class = "eco-status", style = "margin-top:10px;",
            ratio("Precision", ct$tp, ct$tp + ct$fp, ct$precision),
            ratio("Recall (sensitivity)", ct$tp, ct$tp + ct$fn, ct$recall),
            span(style = "margin-right:18px;",
                 tags$strong("F1"), " ", fmt_pct(ct$f1)),
            ratio("Accuracy", ct$tp, ct$n_scored, ct$accuracy)),
        div(class = "eco-note", style = "margin-top:6px;",
            paste("The same colours as the tiles, counted. A disagreement costs",
                  "a false positive and a false negative, since it asserts a",
                  "wrong value and misses the right one; a cell neither side",
                  "filled in is a true negative and drops out of every metric.",
                  "A tile shows the worst outcome among its rows, so the map",
                  "above aggregates where these counts do not -- the chart says",
                  "where to look, the matrix says how much there is."))
      )
    })

    observeEvent(input$matrix_all, matrix_field(NULL))

    # ---- clicking a column name ---------------------------------------------
    # The interactive plot sends the tick label it drew, which is abbreviated;
    # the static one sends coordinates, and a click below the first row is a
    # click on the axis rather than on a tile.
    select_column <- function(field) {
      if (is.null(field) || !field %in% rv$cells$field) return()
      matrix_field(field)
      # The per-column detail panel follows, so one click moves both views.
      updateSelectInput(session, "column", selected = field)
    }

    observeEvent(input$column_click, {
      select_column(field_from_label(input$column_click, unique(heat()$field)))
    })

    # ---- clicking a tile ----------------------------------------------------
    # A tile here covers several records, so there is nothing exact to show in
    # a modal. It opens the paper instead: the overview says where to look, the
    # comparison view is where you look, and there the tiles are single cells.
    open_paper <- function(paper, field = NULL) {
      if (is.null(paper) || !paper %in% rv$scope$papers) return()
      rv$current_paper <- paper
      rv$focus_field <- field
      rv$stage <- "compare"
    }

    observeEvent(plotly::event_data("plotly_click", source = HEATMAP_SOURCE), {
      ev <- plotly::event_data("plotly_click", source = HEATMAP_SOURCE)
      tile <- ecoeval::parse_tile_key(ev$key)
      if (!is.null(tile)) open_paper(tile$row, tile$field)
    })

    observeEvent(input$heatmap_click, {
      d <- heat_plot()$data
      x <- round(input$heatmap_click$x); y <- round(input$heatmap_click$y)
      fields <- levels(d$field); papers <- levels(d$paper)
      if (is.na(x) || is.na(y) || x < 1L || x > length(fields)) return()
      if (y < 1L) return(select_column(fields[[x]]))   # the axis, not a tile
      if (y > length(papers)) return()
      open_paper(papers[[y]], fields[[x]])
    })

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
    output$dl_heatmap <- png_download(
      heat_plot, "paper_column_overview",
      height = max(3.5, 0.28 * heat_papers() + 2.5))
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
        gold_fields = setdiff(names(rv$gold_raw), rv$gold_paper_key$columns),
        collapses = rv$collapses,
        dropped_fields = dropped_fields(rv),
        linkage_fields = identity_fields(rv)
      )
    })

    output$findings <- renderUI({
      f <- findings()
      if (!nrow(f)) return(NULL)
      titles <- c(schema = "Schema and prompt", model = "Model",
                  gold = lab()$gold)
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
        reviewed = rv$reviewed, scope = rv$scope, labels = lab(),
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
