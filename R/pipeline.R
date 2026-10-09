# The evaluation pipeline, one function per stage.
#
# The app's setup screens and evaluate_extraction() call the same functions, so
# a scripted run and a clicked-through one are the same computation:
#
#   load_inputs()          read the record sets, paper lists, and schema
#   choose_paper_keys()    decide which columns identify a paper in each table
#   place_papers()         put every table into canonical form
#   propose_paper_links()  link AI papers to gold papers
#   set_scope()            accept links and compute the papers in scope
#   configure_fields()     field mapping, comparators, identity columns
#   score_evaluation()     normalise, check, align records, score cells
#
# setup_evaluation() chains them, stopping at the first stage that has no
# input to run on. That is what lets run_eval_app() open past the setup screens
# when it was handed everything they would have asked for.

#' Processing steps that can be switched off
#'
#' None of these changes how a cell is scored against the comparator cascade;
#' they are pre-steps and checks around it.
#'
#' * `"normalize"` -- the per-field normalisers, built-in and supplied.
#' * `"conformance"` -- the schema conformance check on both sides.
#' * `"granularity"` -- the check for gold records that collapse together on
#'   the identity columns.
#'
#' @return A character vector of step names.
#' @export
skippable_steps <- function() c("normalize", "conformance", "granularity")

#' @keywords internal
#' @noRd
check_skip <- function(skip) {
  skip <- as.character(skip %||% character(0))
  bad <- setdiff(skip, skippable_steps())
  if (length(bad)) {
    eco_abort(paste0("Unknown step(s) in `skip`: ", paste(bad, collapse = ", "),
                     ". Choose from: ",
                     paste(skippable_steps(), collapse = ", "), "."))
  }
  skip
}

#' @keywords internal
#' @noRd
is_db_path <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    tolower(fs::path_ext(x)) %in% c("db", "sqlite", "sqlite3")
}

#' Read the inputs, from paths or data frames
#'
#' Each record set and paper list may be a path (anything [read_table_any()]
#' reads) or a data frame already in memory -- the latter lets a caller shape
#' records before evaluation, for instance summing line items a document
#' splits. For an ecoextract database with no AI paper list, the database's
#' documents table is used.
#'
#' @param ai,gold The two record sets: paths or data frames.
#' @param schema Path to `schema.json`, or an `ecoeval_schema`.
#' @param ai_papers,gold_papers Optional paper lists: paths or data frames.
#' @param ai_table,gold_table Table names, for database input.
#'
#' @return A list with `schema`, `ai_raw`, `gold_raw`, `ai_papers_raw`,
#'   `gold_papers_raw`, and `inputs` (the paths, for the run configuration;
#'   `NULL` where a data frame was supplied).
#' @export
load_inputs <- function(ai, gold, schema, ai_papers = NULL, gold_papers = NULL,
                        ai_table = NULL, gold_table = NULL) {
  if (is.null(ai)) eco_abort("The AI record set is required.")
  if (is.null(gold)) eco_abort("The gold standard is required.")
  if (is.null(schema)) eco_abort("The schema is required.")

  read <- function(x, table = NULL) {
    if (is.null(x)) return(NULL)
    if (is.data.frame(x)) return(tibble::as_tibble(x))
    read_table_any(x, table)
  }
  path_of <- function(x) if (is.character(x)) x else NULL

  schema_obj <- if (inherits(schema, "ecoeval_schema")) schema else read_schema(schema)
  ai_pap <- read(ai_papers)
  # For a database, the documents table is the paper list for free.
  if (is.null(ai_pap) && is_db_path(ai)) ai_pap <- read_ecoextract_documents(ai)

  list(
    schema = schema_obj,
    ai_raw = read(ai, ai_table),
    gold_raw = read(gold, gold_table),
    ai_papers_raw = ai_pap,
    gold_papers_raw = read(gold_papers),
    inputs = list(
      ai = path_of(ai), ai_table = ai_table,
      gold = path_of(gold), gold_table = gold_table,
      schema = path_of(schema),
      ai_papers = path_of(ai_papers), gold_papers = path_of(gold_papers)
    )
  )
}

#' The tables that need a paper key
#'
#' @param loaded The result of [load_inputs()].
#' @return A named list of data frames: `ai`, `gold`, and whichever paper lists
#'   were supplied.
#' @keywords internal
#' @noRd
key_tables <- function(loaded) {
  Filter(Negate(is.null), list(
    ai = loaded$ai_raw, gold = loaded$gold_raw,
    ai_papers = loaded$ai_papers_raw, gold_papers = loaded$gold_papers_raw
  ))
}

#' Decide which columns identify the paper in each table
#'
#' Keying both sources the same way is what makes their keys comparable, so
#' when nothing is supplied every table is keyed on the best identifier all of
#' them can supply (DOI, then file name, then title, then first author and
#' year), each falling back to its own best when it cannot.
#'
#' @param tables A named list of data frames, from `ai`, `gold`, `ai_papers`,
#'   `gold_papers`.
#' @param paper_key What a caller supplied: `NULL` to detect; a single key
#'   ([paper_key()] or column names) applied to every table; or a named list of
#'   keys by table, where a table left out is detected.
#' @param strategy A key strategy to prefer when detecting, such as `"doi"`.
#'   Defaults to the best one every table shares.
#'
#' @return A named list of `ecoeval_paper_key`s (or `NULL`s), one per table.
#' @export
choose_paper_keys <- function(tables, paper_key = NULL, strategy = NULL) {
  tables <- Filter(Negate(is.null), tables)
  per_table <- is.list(paper_key) && !inherits(paper_key, "ecoeval_paper_key")
  if (per_table) {
    bad <- setdiff(names(paper_key), c("ai", "gold", "ai_papers", "gold_papers"))
    if (length(bad) || is.null(names(paper_key))) {
      eco_abort(paste0("A list `paper_key` is named by table: ai, gold, ",
                       "ai_papers, gold_papers."))
    }
  }
  cand <- lapply(tables, paper_key_candidates)
  shared <- Reduce(intersect, lapply(cand, names)) %||% character(0)
  if (is.null(strategy) || is.na(strategy) || !nzchar(strategy)) {
    strategy <- if (length(shared)) shared[[1L]] else NA_character_
  }

  out <- lapply(names(tables), function(nm) {
    spec <- if (per_table) paper_key[[nm]] else paper_key
    if (!is.null(spec)) {
      key <- as_paper_key(spec, tables[[nm]])
      missing <- setdiff(key$columns, names(tables[[nm]]))
      if (length(missing)) {
        eco_abort(sprintf("The paper key names column(s) the %s table lacks: %s.",
                          nm, paste(missing, collapse = ", ")))
      }
      return(key)
    }
    c_nm <- cand[[nm]]
    if (!length(c_nm)) return(NULL)
    if (!is.na(strategy) && !is.null(c_nm[[strategy]])) c_nm[[strategy]] else c_nm[[1L]]
  })
  stats::setNames(out, names(tables))
}

#' Put every table into canonical form under its paper key
#'
#' Records keep every column at this stage; [configure_fields()] decides which
#' of them are compared. Also runs each source's internal consistency check.
#'
#' @param loaded The result of [load_inputs()].
#' @param keys The result of [choose_paper_keys()].
#'
#' @return A list with `ai`, `gold`, `ai_papers`, `gold_papers` (canonical, or
#'   `NULL`), `metadata_fields` (the paper metadata both lists carry), and
#'   `warnings`.
#' @export
place_papers <- function(loaded, keys) {
  ai <- prepare_records(loaded$ai_raw, keys$ai, prefix = "a")
  gold <- prepare_records(loaded$gold_raw, keys$gold, prefix = "g")

  meta <- function(df) if (is.null(df)) character(0) else paper_metadata_columns(df)
  ai_meta <- meta(loaded$ai_papers_raw)
  gold_meta <- meta(loaded$gold_papers_raw)
  ai_papers <- if (!is.null(loaded$ai_papers_raw)) {
    prepare_papers(loaded$ai_papers_raw, keys$ai_papers, ai_meta)
  }
  gold_papers <- if (!is.null(loaded$gold_papers_raw)) {
    prepare_papers(loaded$gold_papers_raw, keys$gold_papers, gold_meta)
  }

  checks <- dplyr::bind_rows(
    source_consistency(ai, ai_papers, "ai"),
    source_consistency(gold, gold_papers, "gold")
  )
  unplaced <- sum(is.na(ai$.paper)) + sum(is.na(gold$.paper))
  # Keys of different kinds cannot match, so scope would come out empty --
  # say so here rather than let the next stage report nothing to compare.
  mismatched <- !is.null(keys$ai) && !is.null(keys$gold) &&
    !identical(keys$ai$strategy, keys$gold$strategy)
  warnings <- c(
    if (nrow(checks)) paste0(toupper(checks$source), ": ", checks$detail),
    if (unplaced) sprintf(
      paste("%d records carry no paper identifier and cannot be compared.",
            "Check the identifier column."), unplaced),
    if (mismatched) sprintf(
      paste("The two sources are identified differently -- AI by %s, gold",
            "by %s -- so their keys cannot line up. Pick columns of the",
            "same kind on both sides."),
      format(keys$ai), format(keys$gold))
  )

  list(ai = ai, gold = gold, ai_papers = ai_papers, gold_papers = gold_papers,
       metadata_fields = intersect(names(ai_meta), names(gold_meta)),
       warnings = warnings %||% character(0))
}

#' Propose links between the AI's papers and the gold standard's
#'
#' @param placed The result of [place_papers()].
#' @param min_posterior Links at or above this confidence are marked accepted.
#' @return The proposal from [align_papers()]: `ai_paper`, `gold_paper`,
#'   `posterior`, `matcher`, `accepted`.
#' @export
propose_paper_links <- function(placed, min_posterior = 0.85) {
  ai_p <- placed$ai_papers %||% tibble::tibble(.paper = paper_set(placed$ai))
  gold_p <- placed$gold_papers %||% tibble::tibble(.paper = paper_set(placed$gold))
  align_papers(ai_p, gold_p, placed$metadata_fields %||% character(0),
               min_posterior = min_posterior)
}

#' Accept paper links and compute scope
#'
#' Scope is what was accepted, not everything the matcher proposed.
#'
#' @param placed The result of [place_papers()].
#' @param paper_map The accepted links: a data frame with `ai_paper` and
#'   `gold_paper`. Typically a proposal from [propose_paper_links()] filtered to
#'   its `accepted` rows, or supplied directly by the caller.
#'
#' @return A list with `paper_map`, `scope`, `warnings`, and `blocked` (a
#'   message when no paper is in scope, otherwise `NULL`).
#' @export
set_scope <- function(placed, paper_map) {
  if (!is.data.frame(paper_map) ||
      !all(c("ai_paper", "gold_paper") %in% names(paper_map))) {
    eco_abort("`paper_map` needs `ai_paper` and `gold_paper` columns.")
  }
  pm <- tibble::as_tibble(paper_map)
  pm$ai_paper <- as.character(pm$ai_paper)
  pm$gold_paper <- as.character(pm$gold_paper)
  if (!"posterior" %in% names(pm)) pm$posterior <- NA_real_
  if (!"matcher" %in% names(pm)) pm$matcher <- "supplied"
  pm$accepted <- TRUE
  pm <- pm[, c("ai_paper", "gold_paper", "posterior", "matcher", "accepted")]

  ai_set <- paper_set(placed$ai, placed$ai_papers)
  gold_set <- paper_set(placed$gold, placed$gold_papers)
  # A link joins an AI paper to a gold paper, and only links whose two ends
  # both exist count.
  pm <- pm[pm$ai_paper %in% ai_set & pm$gold_paper %in% gold_set, , drop = FALSE]
  scope <- list(
    papers = sort(unique(pm$ai_paper)),
    ai_only = sort(setdiff(ai_set, pm$ai_paper)),
    gold_only = sort(setdiff(gold_set, pm$gold_paper))
  )
  warn <- coverage_warning(!is.null(placed$ai_papers),
                           !is.null(placed$gold_papers), scope)
  list(paper_map = pm, scope = scope, warnings = warn %||% character(0),
       blocked = if (!length(scope$papers)) blocking_condition(scope, character(0)))
}

#' The field mapping, comparator configuration, and identity columns
#'
#' Starts from the schema's defaults and the suggested column mapping, then
#' applies whatever the caller supplied on top. A field is scored only when
#' both sources have a column for it.
#'
#' @param schema An `ecoeval_schema`.
#' @param ai_raw,gold_raw The record tables as read.
#' @param keys The result of [choose_paper_keys()]; the key columns are not
#'   offered for mapping.
#' @param mapping Optional `list(ai = c(field = "column"), gold = c(...))`. A
#'   field named here overrides the suggested column for that side; `NA` maps
#'   the field to nothing. Fields not in the schema are added.
#' @param comparator_config Optional comparator settings: a data frame with a
#'   `field` column and any of `comparator`, `threshold`, `tolerance`,
#'   `set_mode`, `normalizer`, `linkage`, `include`, `ai_col`, `gold_col`.
#'   Rows replace the defaults for their field, so it may cover some fields or
#'   all of them -- a full [default_comparator_config()] tibble works. A blank
#'   `ai_col` or `gold_col` keeps the suggested column.
#' @param linkage_fields Optional identity columns, replacing the schema's
#'   suggestion and any `linkage` in `comparator_config`.
#' @param fields Optional fields to score. Others are left out.
#' @param normalizers Optional named list of functions, by field. Marks those
#'   fields' `normalizer` as `"custom"`.
#'
#' @return A comparator configuration tibble as from
#'   [default_comparator_config()], with `ai_col` and `gold_col` added.
#' @export
configure_fields <- function(schema, ai_raw, gold_raw, keys = list(),
                             mapping = NULL, comparator_config = NULL,
                             linkage_fields = NULL, fields = NULL,
                             normalizers = NULL) {
  key_cols <- function(k) if (is.null(k)) character(0) else k$columns
  ai_cols <- setdiff(names(ai_raw), key_cols(keys$ai))
  gold_cols <- setdiff(names(gold_raw), key_cols(keys$gold))

  check_mapping(mapping, ai_cols, gold_cols)
  extra <- unique(c(names(mapping$ai), names(mapping$gold),
                    comparator_config$field, names(normalizers)))
  all_fields <- unique(c(schema$fields$field, extra))

  cfg <- default_comparator_config(schema, fields = all_fields)
  cfg$ai_col <- suggest_mapping(all_fields, ai_cols)$to
  cfg$gold_col <- suggest_mapping(all_fields, gold_cols)$to

  # Mapping is 1:1, so a column given to a field explicitly is taken from any
  # field the suggestion had given it to.
  apply_side <- function(col, m) {
    if (is.null(m)) return(col)
    col[!cfg$field %in% names(m) & col %in% stats::na.omit(unname(m))] <- NA_character_
    for (f in names(m)) col[[match(f, cfg$field)]] <- unname(m[[f]])
    col
  }
  cfg$ai_col <- apply_side(cfg$ai_col, mapping$ai)
  cfg$gold_col <- apply_side(cfg$gold_col, mapping$gold)

  explicit_include <- FALSE
  if (!is.null(comparator_config)) {
    cc <- tibble::as_tibble(comparator_config)
    if (!"field" %in% names(cc)) eco_abort("`comparator_config` needs a `field` column.")
    bad <- setdiff(names(cc), names(cfg))
    if (length(bad)) {
      eco_abort(paste0("Unknown column(s) in `comparator_config`: ",
                       paste(bad, collapse = ", "), "."))
    }
    bad_cmp <- setdiff(cc$comparator, comparator_choices())
    if ("comparator" %in% names(cc) && length(bad_cmp)) {
      eco_abort(paste0("Unknown comparator(s): ", paste(bad_cmp, collapse = ", "),
                       ". See comparator_choices()."))
    }
    i <- match(cc$field, cfg$field)
    for (col in setdiff(names(cc), "field")) {
      # A blank column here is no opinion, which is what an older saved run
      # without the columns reads back as; `mapping` is how to unmap a field.
      set <- if (col %in% c("ai_col", "gold_col")) !is.na(cc[[col]]) else TRUE
      cfg[[col]][i[set]] <- cc[[col]][set]
    }
    explicit_include <- "include" %in% names(cc)
  }

  if (!is.null(normalizers)) {
    cfg$normalizer[cfg$field %in% names(normalizers)] <- "custom"
  }
  if (!is.null(fields)) {
    unknown <- setdiff(fields, cfg$field)
    if (length(unknown)) {
      eco_abort(paste0("`fields` names field(s) with no column on either side: ",
                       paste(unknown, collapse = ", "), "."))
    }
    cfg$include <- cfg$field %in% fields
  } else if (!explicit_include) {
    cfg$include <- TRUE
  }
  if (!is.null(linkage_fields)) {
    unknown <- setdiff(linkage_fields, cfg$field)
    if (length(unknown)) {
      eco_abort(paste0("`linkage_fields` names unknown field(s): ",
                       paste(unknown, collapse = ", "), "."))
    }
    cfg$linkage <- cfg$field %in% linkage_fields
  }

  # A field neither source has is nothing to compare.
  cfg$include <- as.logical(cfg$include) & !is.na(cfg$ai_col) & !is.na(cfg$gold_col)
  cfg$linkage <- as.logical(cfg$linkage) & cfg$include
  cfg
}

#' @keywords internal
#' @noRd
check_mapping <- function(mapping, ai_cols, gold_cols) {
  if (is.null(mapping)) return(invisible(NULL))
  if (!is.list(mapping) || !all(names(mapping) %in% c("ai", "gold"))) {
    eco_abort("`mapping` is a list with `ai` and/or `gold`, each c(field = \"column\").")
  }
  for (side in names(mapping)) {
    m <- mapping[[side]]
    if (is.null(names(m)) || any(!nzchar(names(m)))) {
      eco_abort(sprintf("`mapping$%s` must be named: c(field = \"column\").", side))
    }
    have <- if (side == "ai") ai_cols else gold_cols
    missing <- setdiff(stats::na.omit(unname(m)), have)
    if (length(missing)) {
      eco_abort(sprintf("`mapping$%s` names column(s) the %s records lack: %s.",
                        side, side, paste(missing, collapse = ", ")))
    }
  }
  invisible(NULL)
}

#' Apply the per-field normalisers to a canonical record table
#'
#' A supplied function wins over the field's built-in normaliser. Every
#' normaliser runs on the value as read and must return a vector the same
#' length.
#'
#' @param records A canonical record tibble.
#' @param config A comparator configuration, for the built-in `normalizer`.
#' @param normalizers Optional named list of functions, by field.
#' @param cache A cache for the built-in LLM normaliser.
#' @param llm Whether the built-in LLM normaliser may run.
#'
#' @return `records` with normalised field columns.
#' @export
apply_normalizers <- function(records, config, normalizers = NULL, cache = NULL,
                              llm = TRUE) {
  for (f in intersect(config$field, names(records))) {
    fn <- normalizers[[f]]
    if (is.function(fn)) {
      before <- records[[f]]
      after <- fn(before)
      if (length(after) != length(before)) {
        eco_abort(sprintf("The normaliser for `%s` returned %d values for %d.",
                          f, length(after), length(before)))
      }
      records[[f]] <- after
      next
    }
    nrm <- config$normalizer[[match(f, config$field)]]
    if (llm && !is.na(nrm) && !nrm %in% c("none", "custom")) {
      records[[f]] <- normalize_values(records[[f]], nrm, cache)
    }
  }
  records
}

#' Normalise, check, align records, and score every cell
#'
#' Records are re-read from `loaded` under the configured mapping, so this
#' stage needs the raw tables rather than the output of [place_papers()].
#'
#' @param loaded The result of [load_inputs()].
#' @param keys The result of [choose_paper_keys()].
#' @param config The result of [configure_fields()].
#' @param paper_map Accepted paper links, from [set_scope()].
#' @param normalizers Optional named list of functions, by field.
#' @param skip Steps to leave out; see [skippable_steps()].
#' @param judge Optional judge from [make_judge()], used by the cascade's top
#'   rung. `NULL` also turns off the built-in LLM normaliser.
#' @param llm Whether the built-in LLM normaliser may run.
#' @param judge_cache,norm_cache Caches from [new_cache()].
#' @param rejected,added,overrides Manual link and cell decisions.
#'
#' @return A list with `ai`, `gold` (canonical and normalised), `ai_shown`,
#'   `gold_shown` (canonical, as read), `conformance`, `collapses`, `pairs`,
#'   `cells`. The cells carry each side's value as read in `ai_original` and
#'   `gold_original`; `ai_value` and `gold_value` are what was compared.
#' @export
score_evaluation <- function(loaded, keys, config, paper_map,
                             normalizers = NULL, skip = character(0),
                             judge = NULL, llm = TRUE,
                             judge_cache = NULL, norm_cache = NULL,
                             rejected = NULL, added = NULL, overrides = NULL) {
  skip <- check_skip(skip)
  use <- config[config$include, , drop = FALSE]
  if (!nrow(use)) {
    eco_abort(blocking_condition(list(papers = paper_map$ai_paper), character(0)))
  }
  if (!any(use$linkage)) {
    eco_abort("Pick at least one identity column for the matcher to link on.")
  }

  ai_shown <- prepare_records(loaded$ai_raw, keys$ai,
                              stats::setNames(use$ai_col, use$field), prefix = "a")
  gold_shown <- prepare_records(loaded$gold_raw, keys$gold,
                                stats::setNames(use$gold_col, use$field), prefix = "g")

  # Normalisation is a pre-matching step: fastLink cannot see through
  # "Myotis lucifugus" against "little brown bat", or a code against the
  # wording it stands for, and those are exactly the fields it depends on most.
  ai <- ai_shown
  gold <- gold_shown
  if (!"normalize" %in% skip) {
    ai <- apply_normalizers(ai, use, normalizers, norm_cache, llm)
    gold <- apply_normalizers(gold, use, normalizers, norm_cache, llm)
  }

  conformance <- if ("conformance" %in% skip) NULL else dplyr::bind_rows(
    check_conformance(ai, loaded$schema, "ai"),
    check_conformance(gold, loaded$schema, "gold")
  )
  linkage <- use$field[use$linkage]
  collapses <- if ("granularity" %in% skip) NULL else
    granularity_check(gold, linkage, "gold")

  pairs <- align_records(ai, gold, linkage, paper_map,
                         rejected = rejected, added = added)
  cells <- score_cells(pairs, ai, gold, use, judge = judge, cache = judge_cache,
                       overrides = overrides,
                       originals = list(ai = ai_shown, gold = gold_shown))
  list(ai = ai, gold = gold, ai_shown = ai_shown, gold_shown = gold_shown,
       conformance = conformance, collapses = collapses, pairs = pairs,
       cells = cells)
}

#' Run the pipeline as far as its inputs allow
#'
#' Chains the stage functions and stops at the first stage it cannot run:
#' either an input is missing and `skip_setup` is `FALSE`, or the stage failed.
#' [run_eval_app()] opens on that stage's screen with everything before it done;
#' [evaluate_extraction()] runs it with `skip_setup = TRUE` and treats stopping
#' early as an error.
#'
#' A setup input counts as supplied when it is not `NULL`. With
#' `skip_setup = TRUE` an input that was not supplied takes the default its
#' screen would have proposed: the detected paper key, the matcher's
#' high-confidence paper links, the suggested field mapping, and the schema's
#' comparators and identity columns.
#'
#' @inheritParams run_eval_app
#' @param judge_cache,norm_cache Caches from [new_cache()].
#' @param rejected,added,overrides Manual link and cell decisions to replay.
#'
#' @return A list with `stage` -- the stage reached: `"load"`, `"metadata"`,
#'   `"papers"`, `"fields"`, or `"scored"` -- and `message` (why it stopped
#'   there, or `NULL`), plus whatever was computed on the way: `loaded`,
#'   `keys`, `placed`, `proposal`, `scoped`, `config`, `scored`, and
#'   `warnings`.
#' @export
setup_evaluation <- function(ai = NULL, gold = NULL, schema = NULL,
                             ai_papers = NULL, gold_papers = NULL,
                             ai_table = NULL, gold_table = NULL,
                             paper_key = NULL, paper_map = NULL,
                             auto_accept = TRUE,
                             mapping = NULL, comparator_config = NULL,
                             linkage_fields = NULL, fields = NULL,
                             normalizers = NULL, skip = character(0),
                             judge = NULL, llm = TRUE, skip_setup = TRUE,
                             judge_cache = new_cache(), norm_cache = new_cache(),
                             rejected = NULL, added = NULL, overrides = NULL) {
  skip <- check_skip(skip)
  if (!is.null(normalizers) &&
      (!is.list(normalizers) || is.null(names(normalizers)) ||
       !all(vapply(normalizers, is.function, logical(1))))) {
    eco_abort("`normalizers` is a named list of functions, one per field.")
  }
  out <- list(stage = "load", message = NULL, warnings = character(0))
  stop_at <- function(stage, message = NULL) {
    out$stage <- stage
    out$message <- message
    out
  }
  attempt <- function(expr) tryCatch(expr, error = function(e) e)

  # ---- load ---------------------------------------------------------------
  if (is.null(ai) || is.null(gold) || is.null(schema)) return(stop_at("load"))
  loaded <- attempt(load_inputs(ai, gold, schema, ai_papers, gold_papers,
                                ai_table, gold_table))
  if (inherits(loaded, "error")) return(stop_at("load", conditionMessage(loaded)))
  out$loaded <- loaded

  # ---- paper keys -----------------------------------------------------------
  if (is.null(paper_key) && !skip_setup) return(stop_at("metadata"))
  keys <- attempt(choose_paper_keys(key_tables(loaded), paper_key))
  if (inherits(keys, "error")) return(stop_at("metadata", conditionMessage(keys)))
  if (is.null(keys$ai) || is.null(keys$gold)) {
    return(stop_at("metadata", "Nothing identifies a paper in one of the record sets."))
  }
  out$keys <- keys
  placed <- attempt(place_papers(loaded, keys))
  if (inherits(placed, "error")) return(stop_at("metadata", conditionMessage(placed)))
  out$placed <- placed
  out$warnings <- c(out$warnings, placed$warnings)

  # ---- paper links and scope ----------------------------------------------
  if (is.null(paper_map)) {
    proposal <- attempt(propose_paper_links(placed))
    if (inherits(proposal, "error")) return(stop_at("papers", conditionMessage(proposal)))
    out$proposal <- proposal
    if (!auto_accept && !skip_setup) return(stop_at("papers"))
    n_low <- sum(!proposal$accepted)
    if (n_low) {
      out$warnings <- c(out$warnings, sprintf(
        paste("%d paper link(s) fell below the confidence cutoff and were left",
              "out of scope. Review them under Align papers."), n_low))
    }
    paper_map <- proposal[proposal$accepted, , drop = FALSE]
  }
  scoped <- attempt(set_scope(placed, paper_map))
  if (inherits(scoped, "error")) return(stop_at("papers", conditionMessage(scoped)))
  out$scoped <- scoped
  out$warnings <- c(out$warnings, scoped$warnings)
  if (!is.null(scoped$blocked)) return(stop_at("papers", scoped$blocked))

  # ---- fields ---------------------------------------------------------------
  config <- attempt(configure_fields(
    loaded$schema, loaded$ai_raw, loaded$gold_raw, keys,
    mapping = mapping, comparator_config = comparator_config,
    linkage_fields = linkage_fields, fields = fields, normalizers = normalizers
  ))
  if (inherits(config, "error")) return(stop_at("fields", conditionMessage(config)))
  out$config <- config
  supplied <- !is.null(mapping) || !is.null(comparator_config) ||
    !is.null(linkage_fields) || !is.null(fields)
  if (!supplied && !skip_setup) return(stop_at("fields"))
  if (!any(config$include)) {
    return(stop_at("fields", blocking_condition(scoped$scope, character(0))))
  }
  if (!any(config$linkage)) {
    return(stop_at("fields",
                   "Pick at least one identity column for the matcher to link on."))
  }

  # ---- score ----------------------------------------------------------------
  scored <- attempt(score_evaluation(
    loaded, keys, config, scoped$paper_map,
    normalizers = normalizers, skip = skip, judge = judge, llm = llm,
    judge_cache = judge_cache, norm_cache = norm_cache,
    rejected = rejected, added = added, overrides = overrides
  ))
  if (inherits(scored, "error")) return(stop_at("fields", conditionMessage(scored)))
  out$scored <- scored
  stop_at("scored")
}
