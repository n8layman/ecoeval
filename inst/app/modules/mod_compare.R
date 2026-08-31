# Stage 5 -- the comparison view. This is the heart of the app.
#
# Alignment review and results are the same screen: judging a link is far
# easier with every field visible than in an abstract table of confidence
# scores. Review is paper by paper, which keeps everything bounded -- a single
# paper holds at most a hundred records and usually far fewer, so the grid is
# small and recompute on every edit is free.
#
# Paper-by-paper is the iteration path, not a mandatory gate. Someone who only
# wants an accuracy figure never has to open this screen.

GRID_LEGEND <- list(
  c("green",  "Matched, values agree"),
  c("yellow", "Matched, values disagree"),
  c("orange", "Gold-only record"),
  c("purple", "AI-only record")
)

mod_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("navigator")),
    eco_panel(
      NULL, NULL,
      div(
        style = "display:flex; gap:14px; align-items:center; flex-wrap:wrap; margin-bottom:8px;",
        checkboxInput(ns("expand"), "Expand every row into two lines",
                      value = TRUE, width = "280px"),
        actionButton(ns("reject"), "Reject this link"),
        actionButton(ns("mark_reviewed"), "Mark paper reviewed", class = "btn-primary")
      ),
      div(class = "eco-legend", lapply(GRID_LEGEND, function(x) {
        span(span(class = "sw",
                  style = sprintf("background:%s;", unname(ecoeval::ecoeval_palette()[[x[[1]]]]))),
             x[[2]])
      }),
      span(span(class = "sw",
                style = "background:#fff; outline:2px dashed #d9534f; outline-offset:-2px;"),
           "Fails schema validation")),
      div(style = "overflow-x:auto;", DT::DTOutput(ns("grid")))
    ),
    uiOutput(ns("link_panel"))
  )
}

mod_compare_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    paper_pairs <- reactive({
      req(rv$pairs, rv$current_paper)
      rv$pairs[rv$pairs$paper == rv$current_paper, , drop = FALSE]
    })
    paper_cells <- reactive({
      req(rv$cells, rv$current_paper)
      rv$cells[rv$cells$paper == rv$current_paper, , drop = FALSE]
    })

    # ---- navigator ---------------------------------------------------------
    output$navigator <- renderUI({
      req(rv$scope)
      papers <- rv$scope$papers
      prog <- ecoeval::progress_summary(rv$cells %||% ecoeval::empty_cells(),
                                        papers, rv$reviewed)
      pp <- paper_pairs()
      n_link <- sum(pp$kind == "pair")
      conf <- pp$posterior[pp$kind == "pair"]
      high <- sum(!is.na(conf) & conf >= 0.85)

      eco_panel(
        NULL, NULL,
        div(
          style = "display:flex; gap:12px; align-items:center; flex-wrap:wrap;",
          actionButton(ns("prev"), "Previous"),
          div(style = "flex:1; min-width:260px;",
              selectInput(ns("paper"), NULL, choices = papers,
                          selected = rv$current_paper, width = "100%")),
          actionButton(ns("nxt"), "Next")
        ),
        div(class = "eco-status",
            sprintf("Alignment: %d of %d links high confidence", high, n_link),
            if (n_link - high > 0) sprintf(" · %d need review", n_link - high),
            " — ",
            sprintf("%d of %d papers judged · %d reviewed",
                    prog$n_judged, prog$n_papers, prog$n_reviewed),
            if (!is.null(corrections())) span(" · ", corrections())
        )
      )
    })

    corrections <- reactive({
      ecoeval::correction_count(rv$rejected, rv$added, rv$overrides)
    })

    observeEvent(input$paper, {
      if (!identical(input$paper, rv$current_paper)) rv$current_paper <- input$paper
    }, ignoreInit = TRUE)

    step <- function(by) {
      papers <- rv$scope$papers
      i <- match(rv$current_paper, papers)
      j <- min(max(i + by, 1L), length(papers))
      rv$current_paper <- papers[[j]]
      updateSelectInput(session, "paper", selected = papers[[j]])
    }
    observeEvent(input$prev, step(-1L))
    observeEvent(input$nxt, step(1L))

    # ---- the grid ----------------------------------------------------------
    grid_data <- reactive({
      cells <- paper_cells()
      pp <- paper_pairs()
      if (!nrow(pp)) return(NULL)
      ident <- identity_fields(rv)
      value_fields <- setdiff(scored_fields(rv), ident)
      violations <- violation_keys(rv)
      expand <- isTRUE(input$expand)

      cell_html <- function(pair_id, field, identity = FALSE) {
        row <- cells[cells$pair_id == pair_id & cells$field == field, , drop = FALSE]
        if (!nrow(row)) return("")
        colour <- unname(ecoeval::state_colour(row$state[[1L]]))
        bad <- any(c(paste("ai", field, ecoeval::canonicalise(row$ai_value[[1L]])),
                     paste("gold", field, ecoeval::canonicalise(row$gold_value[[1L]]))) %in%
                     violations)
        fmt <- function(x) {
          if (is.na(x) || !nzchar(x)) return("<span style='opacity:.35'>--</span>")
          htmltools::htmlEscape(x)
        }
        if (identity) {
          sprintf('<div class="eco-id"><div class="ai">%s</div><div class="gold">%s</div></div>',
                  fmt(row$ai_value[[1L]]), fmt(row$gold_value[[1L]]))
        } else if (expand) {
          sprintf('<div class="eco-cell eco-%s%s"><div class="eco-v ai"><span class="tag">AI</span>%s</div><div class="eco-v gold"><span class="tag">GS</span>%s</div></div>',
                  colour, if (bad) " eco-violation" else "",
                  fmt(row$ai_value[[1L]]), fmt(row$gold_value[[1L]]))
        } else {
          sprintf('<div class="eco-cell eco-%s%s"><div class="eco-v ai">%s</div></div>',
                  colour, if (bad) " eco-violation" else "",
                  fmt(row$ai_value[[1L]]))
        }
      }

      out <- data.frame(Row = seq_len(nrow(pp)), check.names = FALSE)
      out[["Kind"]] <- c(pair = "matched", ai_only = "AI only",
                         gold_only = "gold only")[pp$kind]
      # Identity columns pinned left, rendered as text -- the way a spreadsheet
      # freezes ID columns. That is what makes the scan fast.
      for (f in ident) {
        out[[f]] <- vapply(pp$pair_id, cell_html, character(1), field = f,
                           identity = TRUE)
      }
      for (f in value_fields) {
        out[[f]] <- vapply(pp$pair_id, cell_html, character(1), field = f)
      }
      list(table = out, pair_ids = pp$pair_id,
           fields = c(NA, NA, ident, value_fields))
    })

    output$grid <- DT::renderDT({
      g <- grid_data()
      if (is.null(g)) return(NULL)
      DT::datatable(
        g$table, rownames = FALSE, escape = FALSE,
        selection = list(mode = "single", target = "cell"),
        options = list(
          pageLength = 25, dom = "tip", scrollX = TRUE,
          columnDefs = list(
            list(targets = 0, width = "34px"),
            list(targets = 1, width = "72px")
          )
        )
      )
    }, server = FALSE)

    selected_cell <- reactive({
      sel <- input$grid_cells_selected
      g <- grid_data()
      if (is.null(g) || is.null(sel) || !length(sel) || !nrow(sel)) return(NULL)
      i <- sel[1, 1] + 1L   # DT reports zero-based row and column
      j <- sel[1, 2] + 1L
      field <- g$fields[[j]]
      if (is.na(field)) return(NULL)
      list(pair_id = g$pair_ids[[i]], field = field)
    })

    # ---- the cell modal ----------------------------------------------------
    observeEvent(input$grid_cells_selected, {
      sc <- selected_cell()
      if (is.null(sc)) return()
      row <- rv$cells[rv$cells$pair_id == sc$pair_id & rv$cells$field == sc$field, ]
      if (!nrow(row)) return()
      row <- row[1, ]

      rung_text <- switch(
        row$rung,
        exact = "The two values are byte-identical.",
        normalized = "They agree once trimmed and case-folded.",
        numeric = "Compared as numbers, within the configured tolerance.",
        date = "Parsed as dates, then compared.",
        set = "Compared as sets of values.",
        fuzzy = sprintf("String similarity %.3f.", row$score),
        judge = "The LLM judge decided this one.",
        override = "You decided this one.",
        blank = "One or both sides are blank.",
        unpaired = "This record has no counterpart, so there is nothing to compare.",
        row$rung
      )

      showModal(modalDialog(
        title = sc$field,
        size = "l", easyClose = TRUE,
        div(class = "eco-modal-value",
            div(class = "k", "AI"), div(row$ai_value %||% "--")),
        div(class = "eco-modal-value",
            div(class = "k", "Gold standard"), div(row$gold_value %||% "--")),
        div(class = "eco-status", tags$strong("Decided by: "), row$rung, " — ",
            rung_text),
        if (!is.na(row$rationale))
          div(class = "eco-modal-value", style = "margin-top:10px;",
              div(class = "k", "Rationale"), div(row$rationale)),
        if (isTRUE(row$pending))
          div(class = "eco-warn", style = "margin-top:10px;",
              "The cheap rungs could not settle this one and the judge has not",
              "run yet, so it currently counts as a disagreement."),
        footer = tagList(
          if (row$state %in% c("agree", "disagree", "ai_missing", "gold_missing")) {
            tagList(
              actionButton(ns("say_same"), "They mean the same thing"),
              actionButton(ns("say_different"), "They do not")
            )
          },
          modalButton("Close")
        )
      ))
    }, ignoreInit = TRUE)

    override_cell <- function(agree) {
      sc <- selected_cell()
      if (is.null(sc)) return()
      ov <- rv$overrides
      ov <- ov[!(ov$pair_id == sc$pair_id & ov$field == sc$field), , drop = FALSE]
      rv$overrides <- dplyr::bind_rows(ov, tibble::tibble(
        pair_id = sc$pair_id, field = sc$field, agree = agree))
      rv$cells <- ecoeval::apply_overrides(rv$cells, rv$overrides)
      rv$dirty <- rv$dirty + 1L
      removeModal()
    }
    observeEvent(input$say_same, override_cell(TRUE))
    observeEvent(input$say_different, override_cell(FALSE))

    # ---- the three operations: reject, link, override ----------------------
    observeEvent(input$reject, {
      sc <- selected_cell()
      if (is.null(sc)) {
        showNotification("Click a cell in the row you want to unlink first.",
                         type = "warning")
        return()
      }
      row <- rv$pairs[rv$pairs$pair_id == sc$pair_id, ]
      if (!nrow(row) || row$kind[[1L]] != "pair") {
        showNotification("That row is not a link.", type = "warning")
        return()
      }
      rv$rejected <- dplyr::bind_rows(rv$rejected, tibble::tibble(
        ai_rid = row$ai_rid[[1L]], gold_rid = row$gold_rid[[1L]]))
      realign(rv)
    })

    output$link_panel <- renderUI({
      pp <- paper_pairs()
      ai_only <- pp$pair_id[pp$kind == "ai_only"]
      gold_only <- pp$pair_id[pp$kind == "gold_only"]
      if (!length(ai_only) || !length(gold_only)) {
        return(eco_panel(
          "Linking",
          paste("Linking matters more than rejecting: rejection can only make",
                "the alignment sparser, and the pairs a matcher misses are",
                "exactly the ones whose identifiers disagree -- the ones a",
                "human can recognise as the same record."),
          div(class = "eco-status",
              "This paper has no unpaired rows left on both sides.")))
      }
      label_for <- function(ids, side) {
        cells <- paper_cells()
        ident <- identity_fields(rv)
        stats::setNames(ids, vapply(ids, function(pid) {
          vals <- cells[cells$pair_id == pid & cells$field %in% ident, ]
          v <- if (side == "ai") vals$ai_value else vals$gold_value
          paste(stats::na.omit(v), collapse = " · ")
        }, character(1)))
      }
      eco_panel(
        "Link two unpaired records",
        paste("The pairs a matcher misses are exactly the ones whose",
              "identifiers disagree, and those are the ones a human can",
              "recognise as the same record. Without manual linking you could",
              "only correct in one direction."),
        fluidRow(
          column(5, selectInput(ns("link_ai"), "AI-only row",
                                choices = label_for(ai_only, "ai"), width = "100%")),
          column(5, selectInput(ns("link_gold"), "Gold-only row",
                                choices = label_for(gold_only, "gold"), width = "100%")),
          column(2, div(style = "margin-top:25px;",
                        actionButton(ns("do_link"), "Link", class = "btn-primary")))
        )
      )
    })

    observeEvent(input$do_link, {
      req(input$link_ai, input$link_gold)
      a <- rv$pairs$ai_rid[rv$pairs$pair_id == input$link_ai]
      g <- rv$pairs$gold_rid[rv$pairs$pair_id == input$link_gold]
      if (!length(a) || !length(g) || is.na(a[[1L]]) || is.na(g[[1L]])) return()
      rv$added <- dplyr::bind_rows(rv$added, tibble::tibble(
        ai_rid = a[[1L]], gold_rid = g[[1L]]))
      realign(rv)
    })

    observeEvent(input$mark_reviewed, {
      rv$reviewed <- union(rv$reviewed, rv$current_paper)
      showNotification(sprintf("%s marked reviewed.", rv$current_paper),
                       type = "message")
      step(1L)
    })
  })
}

#' Re-run the matcher and the scorer after a manual link decision
#'
#' All three manual operations recompute every metric immediately. A paper
#' holds at most a hundred records, so this is free.
realign <- function(rv) {
  use <- rv$comparators[rv$comparators$include, , drop = FALSE]
  linkage <- use$field[use$linkage]
  rv$pairs <- ecoeval::align_records(rv$ai, rv$gold, linkage, rv$paper_map,
                                     rejected = rv$rejected, added = rv$added)
  rv$cells <- ecoeval::score_cells(rv$pairs, rv$ai, rv$gold, use,
                                   judge = NULL, cache = rv$judge_cache,
                                   overrides = rv$overrides)
  rv$dirty <- rv$dirty + 1L
}
