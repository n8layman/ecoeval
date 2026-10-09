#' Launch the ecoeval dashboard
#'
#' Evaluates AI extraction output against a human gold standard. All arguments
#' are optional at launch -- the app collects whatever is missing -- but a
#' schema must be supplied before the comparison stage.
#'
#' The app is a shell over a library. Everything it computes is available as
#' plain functions over data frames ([read_schema()], [align_records()],
#' [score_cells()], [field_metrics()], [export_bundle()]), so an evaluation can
#' be scripted without launching anything.
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
#' @param run_config Optional path to a `run_config.json` from an earlier
#'   session. Reload it and you are exactly where you were -- mappings, manual
#'   link decisions, and cached LLM verdicts included.
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Reserved.
#'
#' @return Runs the Shiny application; called for its side effect.
#' @examples
#' \dontrun{
#' # Launch and pick inputs in the app.
#' run_eval_app()
#'
#' # Or supply them up front.
#' run_eval_app(
#'   ai     = system.file("extdata", "ai_records.csv", package = "ecoeval"),
#'   gold   = system.file("extdata", "gold_records.csv", package = "ecoeval"),
#'   schema = system.file("extdata", "schema.json", package = "ecoeval")
#' )
#' }
#' @export
run_eval_app <- function(ai = NULL,
                         gold = NULL,
                         schema = NULL,
                         ai_papers = NULL,
                         gold_papers = NULL,
                         run_config = NULL,
                         launch.browser = TRUE,
                         ...) {
  app_dir <- app_directory()
  if (!nzchar(app_dir)) {
    eco_abort("The app directory is missing -- reinstall ecoeval.")
  }
  for (p in c(ai, gold, schema, ai_papers, gold_papers, run_config)) {
    if (!is.null(p) && !file.exists(p)) eco_abort(paste0("File not found: ", p))
  }
  shiny::shinyOptions(ecoeval_args = list(
    ai = ai, gold = gold, schema = schema,
    ai_papers = ai_papers, gold_papers = gold_papers,
    run_config = run_config
  ))
  shiny::runApp(app_dir, launch.browser = launch.browser)
}

#' Where the Shiny app lives
#'
#' Resolves from the installed package, and from the source tree when running
#' under [pkgload::load_all()].
#'
#' @return A directory path, or `""`.
#' @keywords internal
#' @noRd
app_directory <- function() {
  p <- system.file("app", package = "ecoeval")
  if (nzchar(p) && dir.exists(p)) return(p)
  local <- file.path("inst", "app")
  if (dir.exists(local)) return(normalizePath(local))
  ""
}

#' Run an evaluation end to end without the app
#'
#' The headless path the app is a shell over. Useful for a scripted run, a
#' regression check, or producing an export bundle in CI.
#'
#' @param ai,gold Paths to the two record sets, or record tibbles already in
#'   canonical form.
#' @param schema Path to `schema.json`, or an `ecoeval_schema`.
#' @param ai_papers,gold_papers Optional paper list paths or tibbles.
#' @param paper_key The columns identifying the paper -- see [paper_key()].
#'   Detected per table when `NULL`.
#' @param linkage_fields Linkage fields. Defaults to the schema suggestion.
#' @param judge An optional judge from [make_judge()].
#'
#' @return A list with `schema`, `ai`, `gold`, `scope`, `paper_map`, `config`,
#'   `pairs`, `cells`, `findings` -- the same objects the app holds.
#' @export
evaluate_extraction <- function(ai, gold, schema,
                                ai_papers = NULL, gold_papers = NULL,
                                paper_key = NULL, linkage_fields = NULL,
                                judge = NULL) {
  schema <- if (inherits(schema, "ecoeval_schema")) schema else read_schema(schema)

  ai_raw <- if (is.data.frame(ai)) ai else read_table_any(ai)
  gold_raw <- if (is.data.frame(gold)) gold else read_table_any(gold)

  ai_key <- as_paper_key(paper_key, ai_raw)
  gold_key <- as_paper_key(paper_key, gold_raw)
  key_cols <- function(k) if (is.null(k)) character(0) else k$columns
  ai_map <- suggest_mapping(setdiff(names(ai_raw), key_cols(ai_key)),
                            schema$fields$field)
  gold_map <- suggest_mapping(setdiff(names(gold_raw), key_cols(gold_key)),
                              schema$fields$field)
  to_named <- function(m) stats::setNames(m$from[!is.na(m$to)], m$to[!is.na(m$to)])

  ai_c <- prepare_records(ai_raw, ai_key, to_named(ai_map), prefix = "a")
  gold_c <- prepare_records(gold_raw, gold_key, to_named(gold_map), prefix = "g")

  load_papers <- function(x) {
    if (is.null(x)) return(NULL)
    raw <- if (is.data.frame(x)) x else read_table_any(x)
    prepare_papers(raw, paper_key, paper_metadata_columns(raw))
  }
  ai_p <- load_papers(ai_papers)
  gold_p <- load_papers(gold_papers)

  scope <- compute_scope(paper_set(ai_c, ai_p), paper_set(gold_c, gold_p))
  fields <- intersect(schema$fields$field, intersect(names(ai_c), names(gold_c)))
  blocked <- blocking_condition(scope, fields)
  if (!is.null(blocked)) eco_abort(blocked)

  config <- default_comparator_config(schema, fields = fields,
                                      linkage = linkage_fields)
  linkage <- config$field[config$linkage]
  paper_map <- tibble::tibble(ai_paper = scope$papers, gold_paper = scope$papers)

  pairs <- align_records(ai_c, gold_c, linkage, paper_map)
  cells <- score_cells(pairs, ai_c, gold_c, config, judge = judge,
                       cache = new_cache())
  conformance <- dplyr::bind_rows(
    check_conformance(ai_c, schema, "ai"),
    check_conformance(gold_c, schema, "gold")
  )
  findings <- collect_findings(
    cells, pairs, conformance, schema,
    gold_fields = setdiff(names(gold_c), c(".rid", ".paper")),
    collapses = granularity_check(gold_c, linkage, "gold"),
    dropped_fields = setdiff(
      union(setdiff(names(ai_c), c(".rid", ".paper")),
            setdiff(names(gold_c), c(".rid", ".paper"))),
      fields
    ),
    linkage_fields = linkage
  )
  list(schema = schema, ai = ai_c, gold = gold_c, scope = scope,
       paper_map = paper_map, config = config, pairs = pairs, cells = cells,
       conformance = conformance, findings = findings)
}
