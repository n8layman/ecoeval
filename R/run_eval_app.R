#' Launch the ecoeval dashboard
#'
#' Evaluates AI extraction output against a human gold standard. All arguments
#' are optional at launch -- the app collects whatever is missing -- but a
#' schema must be supplied before the comparison stage.
#'
#' @param ai Path to the AI record set: an ecoextract SQLite database, or a
#'   CSV/Excel file.
#' @param gold Path to the gold standard record set (CSV, Excel, or database).
#' @param schema Path to the `schema.json` the AI extracted against. Required
#'   before comparison; drives comparator defaults, enum conformance checks, and
#'   the default linkage-field suggestion.
#' @param ai_papers,gold_papers Optional paths to paper lists ("papers
#'   processed" / "papers reviewed") as distinct from records found. For an
#'   ecoextract database `ai_papers` is read from the `documents` table.
#'   Supplying these widens scope -- see `DESIGN.md`.
#' @param ... Reserved.
#'
#' @return Runs the Shiny application; called for its side effect.
#' @export
run_eval_app <- function(ai = NULL,
                         gold = NULL,
                         schema = NULL,
                         ai_papers = NULL,
                         gold_papers = NULL,
                         ...) {
  stop("Not yet implemented -- see DESIGN.md and the open issues.")
}
