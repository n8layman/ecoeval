# Loading the five inputs.
#
# ecoeval must work on a plain CSV gold standard and a plain CSV record set.
# Its only hard requirement is a record table plus the schema.json it was
# extracted against -- an ecoextract database is a convenience, not a premise.
# So ecoextract and ecoreview sit in Suggests, and every call into them is
# guarded.

#' Read a table from whatever the user pointed at
#'
#' Accepts CSV, TSV, Excel, RDS, and SQLite. For a database, `table` names the
#' table to read; when it is omitted the largest table is used, which is the
#' record table in every ecoextract database.
#'
#' @param path Path to a file.
#' @param table Table name, for database input.
#'
#' @return A tibble.
#' @export
read_table_any <- function(path, table = NULL) {
  if (!file.exists(path)) eco_abort(paste0("File not found: ", path))
  ext <- tolower(fs::path_ext(path))
  switch(
    ext,
    csv = readr::read_csv(path, show_col_types = FALSE, progress = FALSE),
    tsv = readr::read_tsv(path, show_col_types = FALSE, progress = FALSE),
    txt = readr::read_tsv(path, show_col_types = FALSE, progress = FALSE),
    rds = tibble::as_tibble(readRDS(path)),
    json = tibble::as_tibble(jsonlite::fromJSON(path, flatten = TRUE)),
    xlsx = read_excel_guarded(path),
    xls  = read_excel_guarded(path),
    db = read_db_table(path, table),
    sqlite = read_db_table(path, table),
    sqlite3 = read_db_table(path, table),
    eco_abort(paste0("Unsupported file type: .", ext))
  )
}

#' @keywords internal
#' @noRd
read_excel_guarded <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    eco_abort("Reading Excel needs the readxl package: install.packages('readxl')")
  }
  tibble::as_tibble(readxl::read_excel(path))
}

#' Tables in a SQLite database, largest first
#'
#' @param path Path to a `.db` file.
#' @return A tibble with `table` and `n_rows`.
#' @export
list_db_tables <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tables <- DBI::dbListTables(con)
  if (!length(tables)) {
    return(empty_tbl(table = character(), n_rows = integer()))
  }
  n <- vapply(tables, function(t) {
    as.integer(DBI::dbGetQuery(
      con, sprintf("SELECT COUNT(*) AS n FROM %s", DBI::dbQuoteIdentifier(con, t))
    )$n)
  }, integer(1))
  dplyr::arrange(tibble::tibble(table = tables, n_rows = n), dplyr::desc(.data$n_rows))
}

#' @keywords internal
#' @noRd
read_db_table <- function(path, table = NULL) {
  tabs <- list_db_tables(path)
  if (!nrow(tabs)) eco_abort(paste0("No tables in ", path))
  if (is.null(table)) table <- tabs$table[[1L]]
  if (!table %in% tabs$table) {
    eco_abort(paste0("Table '", table, "' not in ", path, ". Available: ",
                     paste(tabs$table, collapse = ", ")))
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tibble::as_tibble(DBI::dbReadTable(con, table))
}

#' The AI paper list from an ecoextract database
#'
#' For an ecoextract database the paper list is free -- it is the `documents`
#' table, which knows every paper processed regardless of whether extraction
#' produced records. That is what lets the evaluation see papers the AI opened
#' and found nothing in.
#'
#' @param path Path to a `.db` file.
#' @return A tibble, or `NULL` when the database has no documents table.
#' @export
read_ecoextract_documents <- function(path) {
  if (requireNamespace("ecoextract", quietly = TRUE) &&
      is.function(getExportedValue("ecoextract", "get_documents"))) {
    out <- quietly(function() {
      tryCatch(
        tibble::as_tibble(getExportedValue("ecoextract", "get_documents")(path)),
        error = function(e) NULL
      )
    })
    # get_documents() reports some failures by returning an empty frame rather
    # than raising, so an empty result means "fall through", not "no papers".
    if (!is.null(out) && NROW(out) > 0L && NCOL(out) > 0L) return(out)
  }
  tabs <- list_db_tables(path)
  hit <- tabs$table[squash_name(tabs$table) %in% c("documents", "document", "papers", "paper")]
  if (!length(hit)) return(NULL)
  read_db_table(path, hit[[1L]])
}

#' Suggest a 1:1 field mapping between two column sets
#'
#' Pre-populates the mapping screen with exact and fuzzy name matches. Mapping
#' is 1:1 plus "ignore"; many-to-one is not supported and users pre-process.
#'
#' @param from Columns to map from (the gold standard's).
#' @param to Columns to map to (the AI's, or the schema's fields).
#' @param threshold Similarity below which no suggestion is made.
#'
#' @return A tibble with `from`, `to` (`NA` for "ignore"), `score`, `basis`
#'   (`"exact"`, `"squashed"`, or `"fuzzy"`).
#' @export
suggest_mapping <- function(from, to, threshold = 0.8) {
  if (!length(from)) {
    return(empty_tbl(from = character(), to = character(),
                     score = numeric(), basis = character()))
  }
  taken <- character(0)
  rows <- lapply(from, function(f) {
    avail <- setdiff(to, taken)
    if (!length(avail)) {
      return(tibble::tibble(from = f, to = NA_character_, score = 0, basis = "none"))
    }
    if (f %in% avail) {
      taken <<- c(taken, f)
      return(tibble::tibble(from = f, to = f, score = 1, basis = "exact"))
    }
    sq <- match(squash_name(f), squash_name(avail))
    if (!is.na(sq)) {
      taken <<- c(taken, avail[[sq]])
      return(tibble::tibble(from = f, to = avail[[sq]], score = 1, basis = "squashed"))
    }
    s <- similarity(squash_name(f), squash_name(avail))
    best <- which.max(s)
    if (length(best) && !is.na(s[[best]]) && s[[best]] >= threshold) {
      taken <<- c(taken, avail[[best]])
      return(tibble::tibble(from = f, to = avail[[best]], score = s[[best]], basis = "fuzzy"))
    }
    tibble::tibble(from = f, to = NA_character_, score = 0, basis = "none")
  })
  dplyr::bind_rows(rows)
}

#' Put a record table into canonical form
#'
#' Adds the `.rid` row key and the `.paper` scope key, renames columns to
#' canonical field names, and drops everything unmapped. Every downstream
#' function expects records in this shape.
#'
#' @param df A record table as read from disk.
#' @param paper_key The columns identifying the paper: an
#'   [`ecoeval_paper_key`][paper_key()], one or more column names, or `NULL` to
#'   detect them with [suggest_paper_key()].
#' @param mapping A named character vector, `canonical_name = source_column`,
#'   or `NULL` to keep the columns as they are.
#' @param prefix Prefix for generated row keys, so AI and gold keys never
#'   collide.
#'
#' @return A tibble with `.rid`, `.paper`, and the canonical field columns.
#' @export
prepare_records <- function(df, paper_key = NULL, mapping = NULL, prefix = "r") {
  df <- tibble::as_tibble(df)
  key <- as_paper_key(paper_key, df)
  paper <- paper_key_values(df, key)

  out <- tibble::tibble(
    .rid = sprintf("%s%05d", prefix, seq_len(nrow(df))),
    .paper = paper
  )
  if (is.null(mapping)) {
    keep <- setdiff(names(df), if (is.null(key)) character(0) else key$columns)
    for (nm in keep) out[[nm]] <- df[[nm]]
  } else {
    mapping <- mapping[!is.na(mapping) & mapping %in% names(df)]
    for (nm in names(mapping)) out[[nm]] <- df[[mapping[[nm]]]]
  }
  out
}

#' Put a paper list into canonical form
#'
#' @param df A paper table.
#' @param paper_key The columns identifying the paper, as in
#'   [prepare_records()]; `NULL` detects them.
#' @param metadata Named character vector of metadata columns to carry through
#'   for paper alignment, e.g. `c(title = "Title", year = "Year")`.
#'   [paper_metadata_columns()] supplies the usual set.
#' @return A tibble with `.paper` plus the requested metadata columns.
#' @export
prepare_papers <- function(df, paper_key = NULL, metadata = NULL) {
  df <- tibble::as_tibble(df)
  key <- as_paper_key(paper_key, df)
  if (is.null(key) || !all(key$columns %in% names(df))) {
    eco_abort("A paper list needs an identifier column.")
  }
  out <- tibble::tibble(.paper = paper_key_values(df, key))
  if (!is.null(metadata)) {
    metadata <- metadata[!is.na(metadata) & metadata %in% names(df)]
    for (nm in names(metadata)) out[[nm]] <- df[[metadata[[nm]]]]
  }
  out <- out[!is_blank(out$.paper), , drop = FALSE]
  dplyr::distinct(out, .data$.paper, .keep_all = TRUE)
}

#' Each source's paper set
#'
#' Each source's paper set is its paper list when supplied, otherwise the
#' papers appearing in its records. Scope is the intersection either way.
#'
#' @param records A canonical record tibble.
#' @param papers A canonical paper tibble, or `NULL`.
#' @return A character vector of paper identifiers.
#' @export
paper_set <- function(records, papers = NULL) {
  if (!is.null(papers) && nrow(papers)) return(unique(papers$.paper))
  sort(unique(records$.paper[!is.na(records$.paper)]))
}

#' Scope: the papers present in both sources
#'
#' Papers are a filter, not a scored entity. Papers outside the intersection
#' are excluded, never penalised.
#'
#' @param ai_papers,gold_papers Character vectors from [paper_set()].
#' @return A list with `papers` (the intersection), `ai_only`, `gold_only`.
#' @export
compute_scope <- function(ai_papers, gold_papers) {
  list(
    papers = sort(intersect(ai_papers, gold_papers)),
    ai_only = sort(setdiff(ai_papers, gold_papers)),
    gold_only = sort(setdiff(gold_papers, ai_papers))
  )
}

#' The lopsided-coverage warning
#'
#' Paper lists widen scope; they do not change how anything is scored. Only a
#' lopsided pairing needs a warning -- when exactly one side supplies a list,
#' coverage is wider in one direction than the other, so quantify it. The
#' warning is self-extinguishing: it disappears once both lists are present.
#'
#' @param have_ai_list,have_gold_list Whether each source supplied a paper list.
#' @param scope The result of [compute_scope()].
#' @return A character scalar, or `NULL` when no warning is warranted.
#' @export
coverage_warning <- function(have_ai_list, have_gold_list, scope) {
  if (have_ai_list == have_gold_list) return(NULL)
  if (!have_ai_list) {
    sprintf(paste0(
      "The AI has no paper list, so %d papers where it produced records but ",
      "the gold standard has none are outside scope, while papers it ",
      "processed and found nothing in are not visible at all. Coverage is ",
      "wider for over-extraction than under-extraction."
    ), length(scope$ai_only))
  } else {
    sprintf(paste0(
      "The gold standard has no paper list, so papers a reviewer read and ",
      "found nothing in are invisible, while %d papers the AI processed ",
      "without producing records are. Coverage is wider for under-extraction ",
      "than over-extraction."
    ), length(scope$ai_only))
  }
}
