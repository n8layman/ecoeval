# Session state.
#
# Field mappings, comparator config, linkage-field selection, and every manual
# link/reject/override decision are real human labour and must survive a
# refresh. Persisting them is also what makes an evaluation reproducible and
# exportable -- and the session artifact and the export bundle are the same
# object, so save, restore, and download share one implementation.

#' The run configuration format version
#'
#' Bumped when the on-disk shape changes incompatibly, so a future reader can
#' tell what it is looking at.
#' @keywords internal
#' @noRd
RUN_CONFIG_VERSION <- 1L

#' This package's version, without assuming it is installed
#'
#' @return A character scalar; `"unknown"` when running from a loaded source
#'   tree rather than an installed package.
#' @keywords internal
#' @noRd
ecoeval_version <- function() {
  tryCatch(as.character(utils::packageVersion("ecoeval")),
           error = function(e) "unknown")
}

#' An empty run configuration
#'
#' @return A list with every slot the app and the export bundle read.
#' @export
new_run_config <- function() {
  list(
    format_version = RUN_CONFIG_VERSION,
    ecoeval_version = ecoeval_version(),
    created = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    label = NULL,
    # What the two sides were called; see side_labels().
    side_labels = NULL,
    inputs = list(
      ai = NULL, ai_table = NULL, ai_paper_key = NULL,
      gold = NULL, gold_table = NULL, gold_paper_key = NULL,
      schema = NULL,
      ai_papers = NULL, ai_papers_paper_key = NULL,
      gold_papers = NULL, gold_papers_paper_key = NULL
    ),
    record_mapping = list(),
    comparators = list(),
    linkage_fields = character(0),
    paper_map = list(),
    rejected_links = list(),
    added_links = list(),
    cell_overrides = list(),
    reviewed_papers = character(0),
    judge_cache = list(),
    normalize_cache = list(),
    scope = list(papers = character(0), ai_only = character(0),
                 gold_only = character(0)),
    warnings = character(0),
    metrics = list()
  )
}

#' Convert a data frame to and from the JSON row list a config holds
#'
#' `jsonlite` round-trips a zero-row data frame as an empty list, which reads
#' back as `list()` rather than a frame -- so both directions are explicit.
#'
#' @param df A data frame.
#' @return A list of rows.
#' @keywords internal
#' @noRd
df_to_rows <- function(df) {
  if (is.null(df) || !NROW(df)) return(list())
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
}

#' @param rows A list of rows.
#' @param proto A zero-row tibble giving the expected columns.
#' @return A tibble.
#' @keywords internal
#' @noRd
rows_to_df <- function(rows, proto = NULL) {
  if (is.null(rows) || !length(rows)) {
    return(proto %||% tibble::tibble())
  }
  out <- dplyr::bind_rows(lapply(rows, function(r) tibble::as_tibble(lapply(r, unlist))))
  if (!is.null(proto)) {
    for (nm in setdiff(names(proto), names(out))) out[[nm]] <- proto[[nm]][NA_integer_]
    out <- out[, names(proto), drop = FALSE]
  }
  out
}

#' Write a run configuration to disk
#'
#' Reload it and you are exactly where you were -- manual link decisions and
#' cached LLM verdicts included. It is also what the run-over-run diff reads.
#'
#' @param config A run configuration list.
#' @param path Destination path.
#' @return `path`, invisibly.
#' @export
write_run_config <- function(config, path) {
  fs::dir_create(fs::path_dir(path))
  jsonlite::write_json(config, path, auto_unbox = TRUE, pretty = TRUE,
                       null = "null", na = "null")
  invisible(path)
}

#' Read a run configuration from disk
#'
#' @param path Path to a `run_config.json`.
#' @return A run configuration list, with any slots the file omitted filled in
#'   from [new_run_config()].
#' @export
read_run_config <- function(path) {
  if (!file.exists(path)) eco_abort(paste0("No run configuration at ", path))
  cfg <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  base <- new_run_config()
  for (nm in names(cfg)) base[[nm]] <- cfg[[nm]]
  base <- normalise_run_config(base)
  if (!identical(as.integer(base$format_version %||% 1L), RUN_CONFIG_VERSION)) {
    rlang::warn(paste0(
      "run_config.json was written by format version ", base$format_version,
      "; this ecoeval reads version ", RUN_CONFIG_VERSION,
      ". Unrecognised slots are ignored."
    ))
  }
  base
}

#' Restore the vector-shaped slots after a JSON round-trip
#'
#' `jsonlite` reads everything back as a nested list, so the slots the rest of
#' the package treats as plain character vectors have to be flattened again --
#' otherwise a reloaded run hands `align_records()` a list where it expects
#' field names.
#'
#' @param cfg A run configuration list.
#' @return `cfg` with its vector slots and verdict caches restored.
#' @keywords internal
#' @noRd
normalise_run_config <- function(cfg) {
  chr <- function(x) if (is.null(x)) character(0) else as.character(unlist(x))
  cfg$linkage_fields <- chr(cfg$linkage_fields)
  cfg$reviewed_papers <- chr(cfg$reviewed_papers)
  cfg$warnings <- chr(cfg$warnings)
  cfg$scope <- lapply(cfg$scope %||% list(), chr)
  cfg$judge_cache <- lapply(cfg$judge_cache %||% list(), function(v) {
    list(agree = isTRUE(unlist(v$agree)[[1L]]),
         rationale = as.character(unlist(v$rationale) %||% NA_character_))
  })
  cfg$normalize_cache <- lapply(cfg$normalize_cache %||% list(),
                                function(v) as.character(unlist(v))[[1L]])
  if (length(cfg$side_labels)) {
    sl <- lapply(cfg$side_labels, function(v) unlist(v)[[1L]])
    cfg$side_labels <- side_labels(sl$ai %||% "AI", sl$gold %||% "Gold standard",
                                   isTRUE(as.logical(sl$neutral)))
  } else {
    cfg$side_labels <- NULL
  }
  cfg
}

#' Capture the current evaluation state as a run configuration
#'
#' @param config The configuration to update, typically the one held by the app.
#' @param comparators A comparator configuration tibble.
#' @param paper_map A paper alignment tibble.
#' @param rejected,added Manual link decisions.
#' @param overrides Manual cell overrides.
#' @param cache,norm_cache Judge and normaliser caches.
#' @param reviewed Papers a person has reviewed.
#' @param scope The result of [compute_scope()].
#' @param metrics A named list of headline numbers to record with the run.
#' @param labels Side labels to record with the run; see [side_labels()].
#'
#' @return The updated configuration list.
#' @export
capture_run_config <- function(config,
                               comparators = NULL,
                               paper_map = NULL,
                               rejected = NULL,
                               added = NULL,
                               overrides = NULL,
                               cache = NULL,
                               norm_cache = NULL,
                               reviewed = NULL,
                               scope = NULL,
                               metrics = NULL,
                               labels = NULL) {
  if (!is.null(comparators)) {
    config$comparators <- df_to_rows(comparators)
    config$linkage_fields <- comparators$field[comparators$linkage]
  }
  if (!is.null(paper_map)) config$paper_map <- df_to_rows(paper_map)
  if (!is.null(rejected)) config$rejected_links <- df_to_rows(rejected)
  if (!is.null(added)) config$added_links <- df_to_rows(added)
  if (!is.null(overrides)) config$cell_overrides <- df_to_rows(overrides)
  if (!is.null(cache)) config$judge_cache <- cache_as_list(cache)
  if (!is.null(norm_cache)) config$normalize_cache <- cache_as_list(norm_cache)
  if (!is.null(reviewed)) config$reviewed_papers <- as.character(reviewed)
  if (!is.null(scope)) config$scope <- lapply(scope, as.character)
  if (!is.null(metrics)) config$metrics <- metrics
  if (!is.null(labels)) config$side_labels <- unclass(as_side_labels(labels))
  config
}

#' Prototypes for the tables a run configuration stores
#'
#' @return A named list of zero-row tibbles.
#' @export
run_config_protos <- function() {
  list(
    comparators = empty_tbl(field = character(), comparator = character(),
                            threshold = numeric(), tolerance = numeric(),
                            set_mode = character(), normalizer = character(),
                            linkage = logical(), include = logical(),
                            ai_col = character(), gold_col = character()),
    paper_map = empty_tbl(ai_paper = character(), gold_paper = character(),
                          posterior = numeric(), matcher = character(),
                          accepted = logical()),
    links = empty_tbl(ai_rid = character(), gold_rid = character()),
    overrides = empty_tbl(pair_id = character(), field = character(),
                          agree = logical())
  )
}

#' Read the tables back out of a run configuration
#'
#' @param config A run configuration list.
#' @return A named list of tibbles: `comparators`, `paper_map`, `rejected`,
#'   `added`, `overrides`.
#' @export
restore_run_tables <- function(config) {
  p <- run_config_protos()
  list(
    comparators = rows_to_df(config$comparators, p$comparators),
    paper_map = rows_to_df(config$paper_map, p$paper_map),
    rejected = rows_to_df(config$rejected_links, p$links),
    added = rows_to_df(config$added_links, p$links),
    overrides = rows_to_df(config$cell_overrides, p$overrides)
  )
}

#' Count the manual corrections in a run
#'
#' Every one of these moves a number, so a hand-tuned result should say so.
#'
#' @param rejected,added,overrides Manual decision tables.
#' @return A character scalar such as
#'   `"12 links rejected, 5 added, 9 cells overridden"`, or `NULL` when the run
#'   is untouched.
#' @export
correction_count <- function(rejected = NULL, added = NULL, overrides = NULL) {
  n <- c(NROW(rejected), NROW(added), NROW(overrides))
  if (sum(n) == 0L) return(NULL)
  sprintf("%d links rejected, %d added, %d cells overridden", n[[1]], n[[2]], n[[3]])
}

#' Diff two runs
#'
#' Without run-over-run comparison, iterating is blind.
#'
#' @param before,after Run configuration lists, each carrying a `metrics` slot
#'   written by [capture_run_config()].
#' @return A tibble with `metric`, `before`, `after`, `delta`.
#' @export
diff_runs <- function(before, after) {
  keys <- union(names(before$metrics %||% list()), names(after$metrics %||% list()))
  if (!length(keys)) {
    return(empty_tbl(metric = character(), before = numeric(),
                     after = numeric(), delta = numeric()))
  }
  num <- function(m, k) {
    v <- suppressWarnings(as.numeric(m[[k]] %||% NA))
    if (length(v) != 1L) NA_real_ else v
  }
  out <- tibble::tibble(
    metric = keys,
    before = vapply(keys, function(k) num(before$metrics, k), numeric(1)),
    after = vapply(keys, function(k) num(after$metrics, k), numeric(1))
  )
  out$delta <- out$after - out$before
  dplyr::arrange(out, dplyr::desc(abs(.data$delta)))
}
