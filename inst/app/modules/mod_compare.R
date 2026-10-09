# Stage 5 -- the comparison view. This is the heart of the app.
#
# Alignment review and results are the same screen: judging a link is far
# easier with every field visible than in an abstract table of confidence
# scores. Review is paper by paper, which keeps everything bounded -- a single
# paper holds at most a hundred records and usually far fewer, so recompute on
# every edit is free.
#
# This is also the only place the four outcome colours are exact. A tile here is
# one record's value for one column: green means these two values agree, orange
# means the AI asserted one the gold standard does not have. The overview on the
# dashboard cannot say that -- its tiles cover several records at once -- so it
# shades a rate instead and sends you here to see what the colours are made of.
#
# Paper-by-paper is the iteration path, not a mandatory gate. Someone who only
# wants an accuracy figure never has to open this screen.

# Past this many tiles, one trace per tile stops being worth it and the heatmap
# renders as a static plot. A paper with a hundred records reaches it.
RECORD_HEATMAP_MAX_TILES <- 900L
RECORD_HEATMAP_SOURCE <- "eco_record_heatmap"

# What a side said, before any normaliser. A cell table without the column
# predates normalisers, so its compared value is also what was said.
original <- function(cells, side) {
  cells[[paste0(side, "_original")]] %||% cells[[paste0(side, "_value")]]
}

# What one clicked tile shows: what each side said, the sentences each side
# quoted for it, and how the verdict was reached. The quoted text is the thing
# that settles a disagreement -- without it a reader has two values and no way
# to tell which one read the paper correctly.
cell_modal_body <- function(cells, pair_id, field, evidence = NA_character_,
                            config = NULL, violations = character(0),
                            label = NULL, kind = NULL) {
  row <- cells[cells$pair_id == pair_id & cells$field == field, , drop = FALSE]
  if (!nrow(row)) {
    return(div(class = "eco-note", "That cell is no longer in the comparison."))
  }
  row <- row[1, ]

  quote_row <- if (!is.na(evidence) && !identical(evidence, field)) {
    cells[cells$pair_id == pair_id & cells$field == evidence, , drop = FALSE]
  }
  offending <- c(
    AI = paste("ai", field, ecoeval::canonicalise(row$ai_value)) %in% violations,
    `gold standard` = paste("gold", field,
                            ecoeval::canonicalise(row$gold_value)) %in% violations
  )
  verdict <- state_verdict(row$state)
  # What a side said, and -- when a normaliser changed it -- what it was
  # compared as.
  value_block <- function(label, value, compared = value) {
    changed <- !is.na(compared) && !is.na(value) && !identical(compared, value)
    div(class = "eco-modal-value",
        div(class = "k", label),
        div(if (is.na(value) || !nzchar(value))
              span(style = "opacity:.45;", "— nothing here —") else value),
        if (changed) div(class = "eco-note", paste("Compared as:", compared)))
  }


  tagList(
    div(class = "eco-status", style = "margin-bottom:10px;",
        span(class = "sw",
             style = sprintf("background:%s; margin-right:6px;",
                             unname(ecoeval::ecoeval_palette()[[
                               unname(ecoeval::state_colour(row$state))]]))),
        tags$strong(verdict[[1L]]),
        sprintf(" — %s · %s · %s", verdict[[2L]],
                if (length(kind)) kind[[1L]] else "",
                if (length(label)) label[[1L]] else "")),
    value_block("AI", original(row, "ai"), row$ai_value),
    value_block("Gold standard", original(row, "gold"), row$gold_value),
    if (!is.null(quote_row) && nrow(quote_row)) {
      tagList(
        value_block("Supporting sentences — AI", original(quote_row, "ai")[[1L]]),
        value_block("Supporting sentences — gold standard",
                    original(quote_row, "gold")[[1L]])
      )
    },
    div(class = "eco-status", tags$strong("Decided by: "), row$rung, " — ",
        rung_explanation(row$rung, row$score, field_threshold(config, field))),
    if (!is.na(row$rationale))
      div(class = "eco-modal-value", style = "margin-top:10px;",
          div(class = "k", "Rationale"), div(row$rationale)),
    if (any(offending))
      div(class = "eco-warn", style = "margin-top:10px;",
          sprintf("The %s value fails schema validation for this column.",
                  paste(names(offending)[offending], collapse = " and the "))),
    if (isTRUE(row$pending))
      div(class = "eco-warn", style = "margin-top:10px;",
          "The cheap rungs could not settle this one and the judge has not",
          "run yet, so it counts as a disagreement for now. Resolve all",
          "differences to have the judge decide it."),
    if (is.na(evidence))
      div(class = "eco-note", style = "margin-top:10px;",
          paste("No supporting-sentence column is being scored, so there is no",
                "quoted text to show. If the extraction has one, map it in the",
                "record-field stage."))
  )
}

mod_compare_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("navigator")),
    eco_panel(
      NULL, NULL,
      div(
        style = "display:flex; gap:14px; align-items:center; flex-wrap:wrap; margin-bottom:8px;",
        actionButton(ns("mark_reviewed"), "Mark paper reviewed", class = "btn-primary"),
        div(class = "eco-status",
            "Click any tile for both values, the sentences each side quoted,",
            "and what decided it.")
      ),
      uiOutput(ns("heatmap_ui")),
      div(class = "eco-note", style = "margin-top:6px;",
          span(class = "sw",
               style = "background:#fff; outline:2px solid #d9534f; outline-offset:-2px; margin-right:6px;"),
          "A dot marks a value that fails schema validation. Validity is",
          "orthogonal to agreement -- a cell can be both -- so fill carries",
          "agreement and the marker carries validity.")
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
      if (!identical(input$paper, rv$current_paper)) {
        rv$current_paper <- input$paper
        rv$focus_field <- NULL
      }
    }, ignoreInit = TRUE)

    step <- function(by) {
      papers <- rv$scope$papers
      i <- match(rv$current_paper, papers)
      j <- min(max(i + by, 1L), length(papers))
      rv$current_paper <- papers[[j]]
      # Walking to another paper leaves behind the column the overview sent us
      # to; the outline would be pointing at nothing in particular.
      rv$focus_field <- NULL
      updateSelectInput(session, "paper", selected = papers[[j]])
    }
    observeEvent(input$prev, step(-1L))
    observeEvent(input$nxt, step(1L))

    # ---- the heatmap -------------------------------------------------------
    # One tile per record per column, in the order the comparison produced:
    # matched pairs, then AI-only, then gold-only.
    heat <- reactive({
      req(rv$cells, rv$current_paper)
      ecoeval::record_field_outcomes(rv$cells, rv$current_paper,
                                     scored_fields(rv), identity_fields(rv))
    })

    # Cells that fail schema validation, keyed the way the plot marks them.
    heat_violations <- reactive({
      d <- heat()
      if (!nrow(d)) return(character(0))
      keys <- violation_keys(rv)
      bad <- vapply(seq_len(nrow(d)), function(i) {
        any(c(paste("ai", d$field[[i]], ecoeval::canonicalise(d$ai_value[[i]])),
              paste("gold", d$field[[i]], ecoeval::canonicalise(d$gold_value[[i]]))) %in%
              keys)
      }, logical(1))
      unique(paste(d$pair_id[bad], d$field[bad]))
    })

    heat_plot <- function() {
      ecoeval::plot_record_heatmap(heat(), heat_violations(),
                                   focus = rv$focus_field,
                                   paper = rv$current_paper)
    }
    heat_rows <- reactive(length(unique(heat()$label)))

    output$heatmap_ui <- renderUI({
      d <- heat()
      if (!nrow(d)) {
        return(div(class = "eco-note", "Nothing to compare in this paper."))
      }
      h <- sprintf("%dpx", 26 * heat_rows() + 300)
      if (nrow(d) <= RECORD_HEATMAP_MAX_TILES) {
        plotly::plotlyOutput(ns("heatmap"), height = h)
      } else {
        tagList(
          plotOutput(ns("heatmap_static"), height = h,
                     click = ns("heatmap_click")),
          div(class = "eco-note", style = "margin-top:6px;",
              sprintf(paste("%d tiles is too many to make every one hoverable,",
                            "so this one is static. Clicking still works."),
                      nrow(d)))
        )
      }
    })

    output$heatmap <- plotly::renderPlotly({
      interactive_heatmap(heat_plot(), RECORD_HEATMAP_SOURCE, NULL)
    })
    output$heatmap_static <- renderPlot(heat_plot())

    # ---- which cell is open ------------------------------------------------
    # The interactive plot hands back the tile's key; the static one hands back
    # coordinates, which land on a tile because both axes are discrete.
    # The nonce is what makes clicking the same tile twice reopen the modal: a
    # reactiveVal set to an identical value does not fire.
    selected <- reactiveVal(NULL)
    clicks <- reactiveVal(0L)
    select_cell <- function(pair_id, field) {
      clicks(clicks() + 1L)
      selected(list(pair_id = pair_id, field = field, nonce = clicks()))
    }
    selected_cell <- reactive(selected())

    # A new paper invalidates whatever was open, and clears the column the
    # overview asked us to look at once it has been seen.
    observeEvent(rv$current_paper, {
      selected(NULL)
      removeModal()
    }, ignoreInit = TRUE)

    pair_from_label <- function(label) {
      d <- heat()
      hit <- d$pair_id[as.character(d$label) == label]
      if (length(hit)) hit[[1L]] else NULL
    }

    observeEvent(plotly::event_data("plotly_click", source = RECORD_HEATMAP_SOURCE), {
      ev <- plotly::event_data("plotly_click", source = RECORD_HEATMAP_SOURCE)
      tile <- ecoeval::parse_tile_key(ev$key)
      if (is.null(tile)) return()
      select_cell(tile$row, tile$field)
    })

    observeEvent(input$heatmap_click, {
      d <- heat()
      p <- heat_plot()$data
      x <- round(input$heatmap_click$x); y <- round(input$heatmap_click$y)
      fields <- levels(p$field); labels <- levels(p$label)
      if (is.na(x) || is.na(y) || x < 1L || y < 1L ||
          x > length(fields) || y > length(labels)) return()
      pid <- pair_from_label(labels[[y]])
      if (!is.null(pid)) select_cell(pid, fields[[x]])
    })

    # ---- the cell modal ----------------------------------------------------
    observeEvent(selected(), {
      sc <- selected_cell()
      if (is.null(sc)) return()
      row <- rv$cells[rv$cells$pair_id == sc$pair_id & rv$cells$field == sc$field, ]
      if (!nrow(row)) return()
      row <- row[1, ]
      d <- heat()

      showModal(modalDialog(
        title = sc$field,
        size = "l", easyClose = TRUE,
        cell_modal_body(
          rv$cells, sc$pair_id, sc$field,
          evidence = ecoeval::evidence_field(unique(rv$cells$field)),
          config = rv$comparators, violations = violation_keys(rv),
          label = unique(as.character(d$label[d$pair_id == sc$pair_id])),
          kind = unique(d$kind[d$pair_id == sc$pair_id])
        ),
        footer = tagList(
          if (row$state %in% c("agree", "disagree", "ai_missing", "gold_missing")) {
            tagList(
              actionButton(ns("say_same"), "They mean the same thing"),
              actionButton(ns("say_different"), "They do not")
            )
          },
          # Rejecting lives here now: the row this cell belongs to is exactly
          # the link a person has just been given the evidence to doubt.
          if (!is.na(row$ai_rid) && !is.na(row$gold_rid))
            actionButton(ns("reject"), "These are not the same record"),
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
      if (is.null(sc)) return()
      row <- rv$pairs[rv$pairs$pair_id == sc$pair_id, ]
      if (!nrow(row) || row$kind[[1L]] != "pair") {
        showNotification("That row is not a link.", type = "warning")
        return()
      }
      rv$rejected <- dplyr::bind_rows(rv$rejected, tibble::tibble(
        ai_rid = row$ai_rid[[1L]], gold_rid = row$gold_rid[[1L]]))
      selected(NULL)
      removeModal()
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
          v <- original(vals, side)
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
  linkage <- identity_fields(rv)
  rv$pairs <- ecoeval::align_records(rv$ai, rv$gold, linkage, rv$paper_map,
                                     rejected = rv$rejected, added = rv$added)
  rescore(rv)
}
