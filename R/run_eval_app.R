#' Launch the ecoeval dashboard
#'
#' Evaluates AI extraction output against a human gold standard. All arguments
#' are optional at launch -- the app collects whatever is missing.
#'
#' Every input the setup screens ask for can be passed here instead. The app
#' runs the setup stages it was given inputs for and opens on the first one it
#' was not, so a launch with everything supplied opens straight on the
#' comparison view. Pass `skip_setup = TRUE` to take the defaults for anything
#' left out rather than stopping to ask. A stage that fails -- a column that is
#' not there, a paper key the two sides cannot share -- opens on its screen
#' with the reason.
#'
#' The app is a shell over a library. Everything it computes is available as
#' plain functions over data frames ([evaluate_extraction()],
#' [setup_evaluation()], [score_cells()], [export_bundle()]), so an evaluation
#' can be scripted without launching anything.
#'
#' @param ai,gold The two record sets: a path (an ecoextract SQLite database,
#'   CSV, Excel, ...) or a data frame. A data frame lets a caller shape records
#'   before the evaluation, but a saved run cannot record it, so restoring that
#'   run means passing it again.
#' @param schema Path to the `schema.json` the AI extracted against, or an
#'   `ecoeval_schema`. Drives comparator defaults, enum conformance checks, and
#'   the default linkage-field suggestion.
#' @param ai_papers,gold_papers Optional paper lists ("papers processed" /
#'   "papers reviewed") as distinct from records found: paths or data frames.
#'   For an ecoextract database `ai_papers` is read from the `documents` table.
#'   Supplying these widens scope -- see `DESIGN.md`.
#' @param ai_table,gold_table Table names, for database input. Defaults to the
#'   largest table.
#' @param paper_key The columns identifying the paper: a [paper_key()] or
#'   column names applied to every table, or a named list by table (`ai`,
#'   `gold`, `ai_papers`, `gold_papers`). Detected when `NULL`.
#' @param paper_map The paper links to use: a data frame with `ai_paper` and
#'   `gold_paper`, in the identifiers the paper key produces. When `NULL` the
#'   matcher proposes them.
#' @param auto_accept When `paper_map` is `NULL`, accept the matcher's
#'   high-confidence links and leave the rest out of scope without stopping to
#'   review them. A warning says how many were left out.
#' @param mapping Record field mapping: `list(ai = c(field = "column"), gold =
#'   c(field = "column"))`. Fields named here override the suggested mapping;
#'   `NA` maps a field to nothing.
#' @param comparator_config Comparator settings: a data frame with a `field`
#'   column and any of the columns [default_comparator_config()] returns. Rows
#'   replace the defaults for their field, so it may cover some fields or all.
#' @param linkage_fields The identity columns records are matched on. Defaults
#'   to the schema's `x-unique-fields`.
#' @param fields The fields to score. Defaults to every field both sides have.
#' @param normalizers Optional named list of functions, by field, applied to
#'   both sides' values before matching and comparison -- for instance mapping
#'   a gold standard's codes onto the wording the documents use. The grid and
#'   tooltips still show what each side actually said. Functions cannot be
#'   saved in a run configuration, so pass them again when restoring a run.
#' @param skip Processing steps to leave out; see [skippable_steps()].
#'   `"normalize"` turns off the normalisers, built-in and supplied.
#' @param judge The LLM judge for "Resolve all differences": a function from
#'   [make_judge()], or `NULL` to turn every LLM step off -- the judge and the
#'   built-in LLM normaliser. When omitted the app builds one from the schema's
#'   field descriptions, as before. The judge only runs when asked to, so
#'   launching never spends money on it.
#' @param skip_setup When `TRUE`, take the default for every setup input not
#'   supplied rather than opening its screen.
#' @param run_config Optional path to a `run_config.json` from an earlier
#'   session. Reload it and you are exactly where you were -- inputs, mappings,
#'   manual link decisions, and cached LLM verdicts included. Arguments passed
#'   here take precedence over what it holds.
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
#' ex <- function(f) system.file("extdata", f, package = "ecoeval")
#' run_eval_app(ai = ex("ai_records.csv"), gold = ex("gold_records.csv"),
#'              schema = ex("schema.json"))
#'
#' # Supply everything and open straight on the comparison.
#' run_eval_app(
#'   ai = ex("ai_records.csv"), gold = ex("gold_records.csv"),
#'   schema = ex("schema.json"),
#'   paper_key = "doi",
#'   linkage_fields = c("bat_species_scientific_name", "interaction_type"),
#'   normalizers = list(
#'     location_country = function(x) dplyr::recode(x, USA = "United States")
#'   ),
#'   judge = NULL
#' )
#' }
#' @export
run_eval_app <- function(ai = NULL,
                         gold = NULL,
                         schema = NULL,
                         ai_papers = NULL,
                         gold_papers = NULL,
                         ai_table = NULL,
                         gold_table = NULL,
                         paper_key = NULL,
                         paper_map = NULL,
                         auto_accept = TRUE,
                         mapping = NULL,
                         comparator_config = NULL,
                         linkage_fields = NULL,
                         fields = NULL,
                         normalizers = NULL,
                         skip = character(0),
                         judge,
                         skip_setup = FALSE,
                         run_config = NULL,
                         launch.browser = TRUE,
                         ...) {
  app_dir <- app_directory()
  if (!nzchar(app_dir)) {
    eco_abort("The app directory is missing -- reinstall ecoeval.")
  }
  for (p in list(ai, gold, schema, ai_papers, gold_papers, run_config)) {
    if (is.character(p) && !file.exists(p)) eco_abort(paste0("File not found: ", p))
  }
  check_skip(skip)
  judge_mode <- if (missing(judge)) "default" else if (is.null(judge)) "off" else "supplied"
  if (judge_mode == "supplied" && !is.function(judge)) {
    eco_abort("`judge` is a function from make_judge(), or NULL to turn it off.")
  }
  shiny::shinyOptions(ecoeval_args = list(
    ai = ai, gold = gold, schema = schema,
    ai_papers = ai_papers, gold_papers = gold_papers,
    ai_table = ai_table, gold_table = gold_table,
    paper_key = paper_key, paper_map = paper_map, auto_accept = auto_accept,
    mapping = mapping, comparator_config = comparator_config,
    linkage_fields = linkage_fields, fields = fields,
    normalizers = normalizers, skip = skip,
    judge_mode = judge_mode,
    judge = if (judge_mode == "supplied") judge,
    skip_setup = skip_setup,
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
#' regression check, or producing an export bundle in CI. It runs the same
#' stages as the app's setup screens ([setup_evaluation()]), taking the default
#' for anything not supplied.
#'
#' @inheritParams run_eval_app
#' @param judge An optional judge from [make_judge()], run on the cells the
#'   cheaper rungs cannot settle. `NULL` (the default) leaves them pending and
#'   also keeps the built-in LLM normaliser off.
#'
#' @return A list with `schema`, `ai`, `gold`, `scope`, `paper_map`, `config`,
#'   `pairs`, `cells`, `conformance`, `findings` -- the same objects the app
#'   holds.
#' @export
evaluate_extraction <- function(ai, gold, schema,
                                ai_papers = NULL, gold_papers = NULL,
                                paper_key = NULL, linkage_fields = NULL,
                                judge = NULL,
                                paper_map = NULL, auto_accept = TRUE,
                                mapping = NULL, comparator_config = NULL,
                                fields = NULL, normalizers = NULL,
                                skip = character(0),
                                ai_table = NULL, gold_table = NULL) {
  run <- setup_evaluation(
    ai = ai, gold = gold, schema = schema,
    ai_papers = ai_papers, gold_papers = gold_papers,
    ai_table = ai_table, gold_table = gold_table,
    paper_key = paper_key, paper_map = paper_map, auto_accept = auto_accept,
    mapping = mapping, comparator_config = comparator_config,
    linkage_fields = linkage_fields, fields = fields,
    normalizers = normalizers, skip = skip,
    judge = judge, llm = !is.null(judge), skip_setup = TRUE
  )
  if (!identical(run$stage, "scored")) {
    eco_abort(run$message %||% sprintf("The evaluation stopped at the %s stage.",
                                       run$stage))
  }
  for (w in run$warnings) rlang::warn(w)

  config <- run$config
  scored <- run$scored
  linkage <- config$field[config$include & config$linkage]
  findings <- collect_findings(
    scored$cells, scored$pairs, scored$conformance, run$loaded$schema,
    gold_fields = config$field[!is.na(config$gold_col)],
    collapses = scored$collapses,
    dropped_fields = config$field[xor(is.na(config$ai_col), is.na(config$gold_col))],
    linkage_fields = linkage
  )
  list(schema = run$loaded$schema, ai = scored$ai, gold = scored$gold,
       scope = run$scoped$scope, paper_map = run$scoped$paper_map,
       config = config, pairs = scored$pairs, cells = scored$cells,
       conformance = scored$conformance, findings = findings)
}
