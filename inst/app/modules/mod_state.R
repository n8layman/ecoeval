# The state every stage shares, plus the small pure helpers that read it.
#
# One reactiveValues object rather than a chain of module return values: the
# stages are revisitable and each one invalidates the ones after it, which is
# far easier to reason about with a single store.

new_app_state <- function() {
  reactiveValues(
    stage = "load",
    args = list(),
    config = ecoeval::new_run_config(),

    # raw inputs, as read from disk
    ai_raw = NULL, gold_raw = NULL,
    ai_papers_raw = NULL, gold_papers_raw = NULL,
    schema = NULL,

    # which columns identify a paper -- one ecoeval_paper_key per table
    ai_paper_key = NULL, gold_paper_key = NULL,
    ai_papers_key = NULL, gold_papers_key = NULL,
    metadata_fields = character(0),

    # canonical form
    ai = NULL, gold = NULL, ai_papers = NULL, gold_papers = NULL,
    record_mapping = NULL,

    scope = NULL, paper_map = NULL,
    comparators = NULL,

    pairs = NULL, cells = NULL,
    conformance = NULL, collapses = NULL,

    rejected = ecoeval::run_config_protos()$links,
    added = ecoeval::run_config_protos()$links,
    overrides = ecoeval::run_config_protos()$overrides,
    reviewed = character(0),

    judge_cache = ecoeval::new_cache(),
    norm_cache = ecoeval::new_cache(),

    warnings = character(0),
    blocked = NULL,
    current_paper = NULL,
    # The column the overview asked the comparison view to look at, outlined
    # there until the reader moves to another paper.
    focus_field = NULL,
    dirty = 0L
  )
}

#' Which stages are open, and which are already done
#'
#' Stages are revisitable; the rail locks only what genuinely cannot be shown
#' yet.
stage_reachable <- function(rv) {
  loaded <- !is.null(rv$ai_raw) && !is.null(rv$gold_raw) && !is.null(rv$schema)
  mapped_meta <- loaded && !is.null(rv$ai) && !is.null(rv$gold)
  scoped <- mapped_meta && !is.null(rv$scope) && length(rv$scope$papers) > 0L
  configured <- scoped && !is.null(rv$comparators) && any(rv$comparators$linkage)
  scored <- configured && !is.null(rv$cells) && nrow(rv$cells) > 0L

  list(
    load      = list(open = TRUE,       done = loaded),
    metadata  = list(open = loaded,     done = mapped_meta),
    papers    = list(open = mapped_meta, done = scoped),
    fields    = list(open = scoped,     done = configured),
    compare   = list(open = scored,     done = length(rv$reviewed) > 0L),
    dashboard = list(open = scored,     done = scored)
  )
}

#' The fields being scored, in a stable order
scored_fields <- function(rv) {
  if (is.null(rv$comparators)) return(character(0))
  rv$comparators$field[rv$comparators$include]
}

#' The identity columns -- the linkage fields chosen in the mapping stage
identity_fields <- function(rv) {
  if (is.null(rv$comparators)) return(character(0))
  rv$comparators$field[rv$comparators$linkage & rv$comparators$include]
}

#' What the cascade rung that decided a cell actually did
#'
#' Both modals -- the comparison grid's and the heatmap's -- have to explain
#' this, and they should say the same thing. A similarity score means nothing
#' without the cutoff it was measured against, so pass the column's threshold
#' whenever it is to hand.
rung_explanation <- function(rung, score = NA_real_, threshold = NA_real_) {
  fuzzy_text <- function() {
    if (is.na(threshold)) return(sprintf("String similarity %.3f.", score))
    sprintf("String similarity %.3f, %s this column's %.2f cutoff.", score,
            if (!is.na(score) && score >= threshold) "at or above" else "below",
            threshold)
  }
  switch(
    rung,
    exact = "The two values are byte-identical.",
    normalized = "They agree once trimmed and case-folded.",
    numeric = "Compared as numbers, within the configured tolerance.",
    date = "Parsed as dates, then compared.",
    set = "Compared as sets of values.",
    fuzzy = fuzzy_text(),
    judge = "The LLM judge decided this one.",
    override = "You decided this one.",
    blank = "One or both sides are blank.",
    unpaired = "This record has no counterpart, so there is nothing to compare.",
    rung
  )
}

#' A column's fuzzy cutoff, when there is a comparator configuration to ask
#'
#' @return A number, or `NA` when the column or the configuration is unknown.
field_threshold <- function(config, field) {
  if (is.null(config) || !NROW(config) || !"threshold" %in% names(config)) {
    return(NA_real_)
  }
  hit <- config$threshold[config$field == field]
  if (!length(hit)) NA_real_ else as.numeric(hit[[1L]])
}

#' How a cell state reads in words, and what it costs the accounting
#'
#' The colour says it, but a modal should not make anyone decode a colour.
state_verdict <- function(state) {
  switch(
    state,
    agree = c("They agree", "true positive"),
    disagree = c("They differ", "false positive and false negative"),
    ai_missing = c("Only in the gold standard", "false negative"),
    gold_only = c("Only in the gold standard", "false negative"),
    gold_missing = c("Only in the AI", "false positive"),
    ai_only = c("Only in the AI", "false positive"),
    blank = c("Neither side has a value", "true negative, dropped from the metrics"),
    c(state, "")
  )
}

# A clicked axis label comes back as the plot drew it, which may be shortened
# with a trailing ellipsis, so match on what is left of the front of it.
field_from_label <- function(label, fields) {
  label <- trimws(sub("\u2026$", "", as.character(label)[[1L]]))
  if (!nzchar(label)) return(NULL)
  hit <- fields[fields == label]
  if (!length(hit)) hit <- fields[startsWith(fields, label)]
  if (length(hit)) hit[[1L]] else NULL
}

# The interactive heatmap: hoverable tiles, clickable tiles, clickable column
# names. Tiles come back through plotly's own click event, which only reaches
# the server for a given source once that event is registered on the widget --
# without event_register() plotly warns and event_data() stays empty.
interactive_heatmap <- function(p, source, input_id) {
  w <- tryCatch(plotly::ggplotly(p, tooltip = "text", source = source),
                error = function(e) plotly::ggplotly(p))
  w <- plotly::event_register(w, "plotly_click")
  clickable_ticks(w, input_id)
}

# Column names are clickable in the interactive heatmap. Plotly emits click
# events for data points but not for axis labels, so bind one delegated
# handler to the widget: delegated, because plotly rebuilds its ticks on every
# resize and a handler attached to the tick itself would not survive that.
clickable_ticks <- function(widget, input_id) {
  # Only the overview has anything to say about a column name; the per-paper
  # heatmap passes NULL and keeps its ticks inert.
  if (is.null(input_id)) return(widget)
  htmlwidgets::onRender(widget, sprintf("
    function(el) {
      el.classList.add('eco-clickable-ticks');
      el.addEventListener('click', function(e) {
        var t = e.target;
        if (!t || !t.parentNode || !t.parentNode.classList) return;
        if (!t.parentNode.classList.contains('xtick')) return;
        Shiny.setInputValue('%s', t.textContent, {priority: 'event'});
      }, true);
    }", input_id))
}

#' A percentage, or a dash when there is nothing to show
fmt_pct <- function(x, digits = 1) {
  if (length(x) != 1L || is.na(x)) return("--")
  sprintf(paste0("%.", digits, "f%%"), 100 * x)
}

#' A headline figure tile
figure_tile <- function(value, key, sub = NULL) {
  div(class = "eco-figure",
      div(class = "k", key),
      div(class = "v", value),
      if (!is.null(sub)) div(class = "s", sub))
}

#' A titled panel with an explanatory note
eco_panel <- function(title, note = NULL, ...) {
  div(class = "eco-panel",
      if (!is.null(title)) h3(title),
      if (!is.null(note)) div(class = "eco-note", note),
      ...)
}

#' Cells that fail schema validation, as a fast lookup key
#'
#' Marked in the grid rather than coloured: validity is orthogonal to match
#' state, a cell can be both, and either side's value may be the offender.
violation_keys <- function(rv) {
  if (is.null(rv$conformance) || !nrow(rv$conformance)) return(character(0))
  paste(rv$conformance$source, rv$conformance$field,
        ecoeval::canonicalise(rv$conformance$value))
}
