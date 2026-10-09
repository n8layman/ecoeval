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

    # canonical form. ai and gold hold the values compared, after any
    # normaliser; ai_shown and gold_shown the values as read, for display.
    ai = NULL, gold = NULL, ai_papers = NULL, gold_papers = NULL,
    ai_shown = NULL, gold_shown = NULL,
    record_mapping = NULL,

    scope = NULL, paper_map = NULL,
    # The matcher's paper-link proposal when it ran at launch, so the review
    # screen shows what was accepted on the user's behalf.
    paper_proposal = NULL,
    comparators = NULL,
    # Field configuration supplied at launch but not yet applied: what the
    # mapping screen starts from instead of the schema's defaults.
    field_seed = NULL,

    # Options only run_eval_app() can set.
    normalizers = NULL,
    skip = character(0),
    judge_mode = "default",
    judge = NULL,

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

#' The stage screen each setup_evaluation() stage opens on
SETUP_STAGE_SCREEN <- c(load = "load", metadata = "metadata", papers = "papers",
                        fields = "fields", scored = "compare")

#' Copy a setup_evaluation() result into the app state
#'
#' Everything the pipeline computed is put where the stage screens would have
#' put it, and the app opens on the stage it stopped at.
seed_state <- function(rv, run) {
  if (!is.null(run$loaded)) {
    rv$schema <- run$loaded$schema
    rv$ai_raw <- run$loaded$ai_raw
    rv$gold_raw <- run$loaded$gold_raw
    rv$ai_papers_raw <- run$loaded$ai_papers_raw
    rv$gold_papers_raw <- run$loaded$gold_papers_raw
    rv$config$inputs[names(run$loaded$inputs)] <- run$loaded$inputs
  }
  if (!is.null(run$keys)) {
    rv$ai_paper_key <- run$keys$ai
    rv$gold_paper_key <- run$keys$gold
    rv$ai_papers_key <- run$keys$ai_papers
    rv$gold_papers_key <- run$keys$gold_papers
    record_paper_keys(rv)
  }
  if (!is.null(run$placed)) {
    rv$ai <- run$placed$ai
    rv$gold <- run$placed$gold
    rv$ai_papers <- run$placed$ai_papers
    rv$gold_papers <- run$placed$gold_papers
    rv$metadata_fields <- run$placed$metadata_fields
  }
  if (!is.null(run$proposal)) rv$paper_proposal <- run$proposal
  if (!is.null(run$scoped)) {
    rv$paper_map <- run$scoped$paper_map
    rv$scope <- run$scoped$scope
  }
  if (!is.null(run$config)) rv$field_seed <- run$config
  if (!is.null(run$scored)) apply_scored(rv, run$scored, run$config)
  rv$warnings <- unique(c(rv$warnings, run$warnings))
  # Zero paper overlap blocks; why any other stage stopped is the caller's to
  # say, since it is news once rather than a standing warning.
  rv$blocked <- run$scoped$blocked
  rv$stage <- SETUP_STAGE_SCREEN[[run$stage]]
  invisible(rv)
}

#' Put a score_evaluation() result into the app state
apply_scored <- function(rv, scored, config) {
  rv$ai <- scored$ai
  rv$gold <- scored$gold
  rv$ai_shown <- scored$ai_shown
  rv$gold_shown <- scored$gold_shown
  rv$conformance <- scored$conformance
  rv$collapses <- scored$collapses
  rv$pairs <- scored$pairs
  rv$cells <- scored$cells
  rv$comparators <- config
  rv$field_seed <- config
  rv$blocked <- NULL
  if (is.null(rv$current_paper) || !rv$current_paper %in% rv$scope$papers) {
    rv$current_paper <- rv$scope$papers[[1L]]
  }
  rv$dirty <- rv$dirty + 1L
  invisible(rv)
}

#' Record the paper keys in the run configuration
record_paper_keys <- function(rv) {
  key_cols <- function(k) if (is.null(k)) NULL else k$columns
  rv$config$inputs$ai_paper_key <- key_cols(rv$ai_paper_key)
  rv$config$inputs$gold_paper_key <- key_cols(rv$gold_paper_key)
  rv$config$inputs$ai_papers_paper_key <- key_cols(rv$ai_papers_key)
  rv$config$inputs$gold_papers_paper_key <- key_cols(rv$gold_papers_key)
}

#' What setup_evaluation() is run with at launch
#'
#' The arguments to run_eval_app() win; a restored run fills in what they leave
#' out -- its input paths, paper keys, paper links, and field configuration --
#' so reloading one lands where it was left.
launch_inputs <- function(args, config, restored = NULL) {
  inp <- config$inputs %||% list()
  first <- function(...) {
    for (x in list(...)) if (!is.null(x) && length(x)) return(x)
    NULL
  }
  saved_keys <- Filter(Negate(is.null), list(
    ai = unlist(inp$ai_paper_key), gold = unlist(inp$gold_paper_key),
    ai_papers = unlist(inp$ai_papers_paper_key),
    gold_papers = unlist(inp$gold_papers_paper_key)
  ))
  nonempty <- function(df) if (!is.null(df) && NROW(df)) df
  restored_cfg <- nonempty(restored$comparators)
  list(
    ai = first(args$ai, unlist(inp$ai)),
    gold = first(args$gold, unlist(inp$gold)),
    schema = first(args$schema, unlist(inp$schema)),
    ai_papers = first(args$ai_papers, unlist(inp$ai_papers)),
    gold_papers = first(args$gold_papers, unlist(inp$gold_papers)),
    ai_table = first(args$ai_table, unlist(inp$ai_table)),
    gold_table = first(args$gold_table, unlist(inp$gold_table)),
    paper_key = first(args$paper_key, if (length(saved_keys)) saved_keys),
    paper_map = first(args$paper_map, nonempty(restored$paper_map)),
    auto_accept = args$auto_accept %||% TRUE,
    mapping = args$mapping,
    comparator_config = first(args$comparator_config, restored_cfg),
    linkage_fields = args$linkage_fields,
    fields = args$fields,
    skip_setup = isTRUE(args$skip_setup)
  )
}

#' Run the setup stages at session start and seed the state from them
#'
#' Runs once, outside any reactive context, hence the isolate().
#'
#' @param rv The app state, with `args` and any restored run already in it.
#' @param restored Tables restored from a run configuration, or `NULL`.
#' @return The setup_evaluation() result, or `NULL` when there was nothing to
#'   run on.
launch_state <- function(rv, restored = NULL) {
  isolate({
    launch <- launch_inputs(rv$args, rv$config, restored)
    if (is.null(launch$ai) && is.null(launch$gold) && is.null(launch$schema)) {
      return(NULL)
    }
    run <- tryCatch(
      do.call(ecoeval::setup_evaluation, c(launch, list(
        normalizers = rv$normalizers, skip = rv$skip, llm = llm_allowed(rv),
        judge_cache = rv$judge_cache, norm_cache = rv$norm_cache,
        rejected = rv$rejected, added = rv$added, overrides = rv$overrides
      ))),
      error = function(e) list(stage = "load", message = conditionMessage(e))
    )
    seed_state(rv, run)
    run
  })
}

#' The raw inputs in the shape the pipeline's stage functions take
loaded_inputs <- function(rv) {
  list(schema = rv$schema, ai_raw = rv$ai_raw, gold_raw = rv$gold_raw,
       ai_papers_raw = rv$ai_papers_raw, gold_papers_raw = rv$gold_papers_raw)
}

#' The paper keys in the shape the pipeline's stage functions take
current_keys <- function(rv) {
  list(ai = rv$ai_paper_key, gold = rv$gold_paper_key,
       ai_papers = rv$ai_papers_key, gold_papers = rv$gold_papers_key)
}

#' Whether the LLM steps are allowed to run
llm_allowed <- function(rv) !identical(rv$judge_mode, "off")

#' The judge for "Resolve all differences"
#'
#' The one supplied to run_eval_app() when there is one, nothing when the LLM
#' was turned off, otherwise one built from the schema's field descriptions.
current_judge <- function(rv) {
  switch(
    rv$judge_mode,
    off = NULL,
    supplied = rv$judge,
    ecoeval::make_judge(
      descriptions = stats::setNames(rv$schema$fields$description,
                                     rv$schema$fields$field)
    )
  )
}

#' Re-score every cell against the current pairs
#'
#' Every rescore goes through here so the values shown -- what each side said
#' before any normaliser -- travel with the cells.
rescore <- function(rv, judge = NULL, progress = NULL) {
  use <- rv$comparators[rv$comparators$include, , drop = FALSE]
  originals <- if (!is.null(rv$ai_shown)) list(ai = rv$ai_shown, gold = rv$gold_shown)
  rv$cells <- ecoeval::score_cells(rv$pairs, rv$ai, rv$gold, use,
                                   judge = judge, cache = rv$judge_cache,
                                   overrides = rv$overrides, progress = progress,
                                   originals = originals)
  rv$dirty <- rv$dirty + 1L
  invisible(rv)
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
