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

    # which column holds what
    ai_paper_col = NULL, gold_paper_col = NULL,
    ai_papers_col = NULL, gold_papers_col = NULL,
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
