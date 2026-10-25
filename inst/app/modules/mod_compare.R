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

# The sentences one record quoted, read from its side's table as loaded rather
# than from the scored cells. The quote column is usually extraction-only -- a
# reference rarely records one -- so it is never mapped or scored, and looking
# for it among the cells would find nothing for exactly the records where it
# matters. A record's `.rid` is its row in the table it came from (see
# prepare_records()). NA when the side has no quote column, the table lacks
# the one named, or there is no record.
record_quote <- function(raw, rid, col) {
  row <- suppressWarnings(as.integer(sub("^\\D+", "", rid %||% NA_character_)))
  if (is.null(raw) || is.na(col) || !col %in% names(raw) || length(row) != 1L ||
      is.na(row) || row > nrow(raw)) {
    return(NA_character_)
  }
  value <- as.character(raw[[col]][[row]])
  if (isTRUE(ecoeval::is_blank(value))) NA_character_ else value
}

# Both sides' quotes for one pair, with which sides have a quote column at all
# and which were named one their records do not have. `cols` is an
# evidence_columns() result; found by name when NULL.
pair_quotes <- function(ai_raw, gold_raw, ai_rid, gold_rid, cols = NULL) {
  cols <- cols %||% ecoeval::evidence_columns(NULL, names(ai_raw), names(gold_raw))
  sides <- c(ai = !is.null(ai_raw), gold = !is.null(gold_raw)) & !is.na(cols)
  found <- c(ai = isTRUE(cols[["ai"]] %in% names(ai_raw)),
             gold = isTRUE(cols[["gold"]] %in% names(gold_raw)))
  list(ai = record_quote(ai_raw, ai_rid, cols[["ai"]]),
       gold = record_quote(gold_raw, gold_rid, cols[["gold"]]),
       sides = sides, missing = sides & !found, cols = cols)
}

# One supporting passage as HTML. Passages are OCR markdown, so they are
# rendered as markdown. A table row quoted on its own -- no header, no
# delimiter line -- is not a markdown table and would read as a run of pipes,
# so pipe rows with nothing else around them are drawn as a table directly.
# Raw HTML in a passage is shown as text, never run.
quote_html <- function(text) {
  md <- function(x) {
    commonmark::markdown_html(gsub("<", "&lt;", x, fixed = TRUE),
                              hardbreaks = TRUE, extensions = TRUE)
  }
  lines <- trimws(strsplit(text, "\n")[[1L]])
  lines <- lines[nzchar(lines)]
  is_delim <- grepl("^\\|?\\s*:?-{3,}:?\\s*(\\|\\s*:?-{3,}:?\\s*)*\\|?$", lines)
  if (length(lines) && all(grepl("|", lines, fixed = TRUE)) && !any(is_delim)) {
    rows <- vapply(lines, function(l) {
      cells <- strsplit(sub("^\\|", "", sub("\\|$", "", l)), "|", fixed = TRUE)[[1L]]
      inline <- vapply(trimws(cells), function(c) {
        sub("^<p>(.*)</p>\n?$", "\\1", md(c))
      }, character(1))
      paste0("<tr>", paste0("<td>", inline, "</td>", collapse = ""), "</tr>")
    }, character(1))
    return(HTML(paste0("<table>", paste(rows, collapse = ""), "</table>")))
  }
  HTML(md(text))
}

# What one clicked tile shows: what each side said, the sentences each side
# quoted for it, and how the verdict was reached. The quoted text is the thing
# that settles a disagreement -- without it a reader has two values and no way
# to tell which one read the paper correctly.
#
# `quotes` is a pair_quotes() result. Without one, the quotes come from the
# `evidence` column among the scored cells, when there is one.
cell_modal_body <- function(cells, pair_id, field, evidence = NA_character_,
                            config = NULL, violations = character(0),
                            label = NULL, kind = NULL, quotes = NULL,
                            judge_mode = "default") {
  row <- cells[cells$pair_id == pair_id & cells$field == field, , drop = FALSE]
  if (!nrow(row)) {
    return(div(class = "eco-note", "That cell is no longer in the comparison."))
  }
  row <- row[1, ]

  if (is.null(quotes) && !is.na(evidence)) {
    q <- cells[cells$pair_id == pair_id & cells$field == evidence, , drop = FALSE]
    if (nrow(q)) {
      quotes <- list(ai = original(q, "ai")[[1L]], gold = original(q, "gold")[[1L]],
                     sides = c(ai = TRUE, gold = TRUE))
    }
  }
  # The column being clicked is not repeated as its own evidence block.
  if (!is.na(evidence) && identical(evidence, field)) quotes <- NULL
  if (!is.null(quotes) && !any(quotes$sides)) quotes <- NULL
  # ecoextract stores the quotes as a JSON array; one passage per sentence.
  if (!is.null(quotes)) {
    quotes$ai <- ecoeval::quote_sentences(quotes$ai)
    quotes$gold <- ecoeval::quote_sentences(quotes$gold)
  }
  offending <- stats::setNames(c(
    paste("ai", field, ecoeval::canonicalise(row$ai_value)) %in% violations,
    paste("gold", field, ecoeval::canonicalise(row$gold_value)) %in% violations
  ), c(lab()$ai, lab()$gold))
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
  quote_block <- function(label, sentences) {
    div(class = "eco-modal-value",
        div(class = "k", label),
        if (!length(sentences)) span(style = "opacity:.45;", "— nothing here —")
        else lapply(sentences, function(s) div(class = "eco-quote", quote_html(s))))
  }
  # One side's quotes -- or, when the column named for it is not in its
  # records, a notice saying so, so a typo does not just show nothing.
  side_quotes <- function(side) {
    if (!quotes$sides[[side]]) return(NULL)
    title <- paste("Supporting sentences —", lab()[[side]])
    if (!isTRUE(quotes$missing[[side]])) return(quote_block(title, quotes[[side]]))
    div(class = "eco-modal-value",
        div(class = "k", title),
        div(class = "eco-warn", sprintf(paste(
          "No '%s' column in the %s records, so there are no supporting",
          "sentences to show. Set evidence_col in run_eval_app() to the",
          "supporting-sentence column."), quotes$cols[[side]], lab()[[side]])))
  }


  tagList(
    div(class = "eco-status", style = "margin-bottom:10px;",
        span(class = "sw",
             style = sprintf("background:%s; margin-right:6px;",
                             unname(ecoeval::ecoeval_palette()[[
                               unname(ecoeval::state_colour(row$state))]]))),
        tags$strong(verdict[[1L]]),
        sprintf(" — %s · %s · %s", verdict[[2L]],
                if (length(kind)) ecoeval::record_kind_label(kind[[1L]]) else "",
                if (length(label)) label[[1L]] else "")),
    value_block(lab()$ai, original(row, "ai"), row$ai_value),
    value_block(lab()$gold, original(row, "gold"), row$gold_value),
    if (!is.null(quotes)) tagList(side_quotes("ai"), side_quotes("gold")),
    div(class = "eco-status", tags$strong("How they were compared: "),
        how_compared(row, field_threshold(config, field))),
    if (!is.na(row$rationale))
      div(class = "eco-modal-value", style = "margin-top:10px;",
          div(class = "k", "The AI reviewer's reasoning"), div(row$rationale)),
    if (any(offending))
      div(class = "eco-warn", style = "margin-top:10px;",
          sprintf("The %s value fails schema validation for this column.",
                  paste(names(offending)[offending], collapse = " and the "))),
    if (isTRUE(row$pending))
      div(class = "eco-warn", style = "margin-top:10px;",
          pending_note(judge_mode, ecoeval::judge_available())),
    if (is.null(quotes) && is.na(evidence))
      div(class = "eco-note", style = "margin-top:10px;",
          paste("Neither side has a supporting-sentence column, so there is no",
                "quoted text to show."))
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

    observeEvent(plotly_click(RECORD_HEATMAP_SOURCE), {
      ev <- plotly_click(RECORD_HEATMAP_SOURCE)
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
          quotes = pair_quotes(rv$ai_raw, rv$gold_raw, row$ai_rid, row$gold_rid,
                               evidence_cols(rv)),
          config = rv$comparators, violations = violation_keys(rv),
          judge_mode = rv$judge_mode,
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
          column(5, selectInput(ns("link_ai"), paste(lab()$ai, "only row"),
                                choices = label_for(ai_only, "ai"), width = "100%")),
          column(5, selectInput(ns("link_gold"), paste(lab()$gold, "only row"),
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
