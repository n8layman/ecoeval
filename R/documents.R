# The source documents.
#
# When two values disagree, the deciding question is what the paper actually
# says. An ecoextract database keeps each paper's OCR markdown, and the
# extraction's own reasoning, in its documents table, so for a database input
# the text is there for the reading: find where each side's value -- and the
# sentence each side quoted -- appears in it, and show those passages.

#' The text columns an ecoextract documents table can carry
#' @keywords internal
#' @noRd
DOCUMENT_TEXT_COLUMNS <- c("document_content", "extraction_reasoning",
                           "refinement_reasoning")

#' Read each paper's OCR text from an ecoextract database
#'
#' Reads the identifying columns and the text columns of the documents table,
#' leaving out the page images, which are large and not needed to read the
#' text.
#'
#' @param path Path to an ecoextract `.db` file.
#' @return A tibble with the documents table's identifying columns plus
#'   `document_content` (the OCR markdown) and, when present,
#'   `extraction_reasoning` and `refinement_reasoning`; `NULL` when the
#'   database has no documents table or no OCR text.
#' @export
read_ecoextract_texts <- function(path) {
  tabs <- list_db_tables(path)
  hit <- tabs$table[squash_name(tabs$table) %in% c("documents", "document")]
  if (!length(hit)) return(NULL)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cols <- DBI::dbListFields(con, hit[[1L]])
  if (!"document_content" %in% cols) return(NULL)
  keep <- setdiff(cols, c("ocr_images", "ocr_log", "metadata_log",
                          "extraction_log", "refinement_log", "bibliography"))
  sql <- sprintf("SELECT %s FROM %s",
                 paste(DBI::dbQuoteIdentifier(con, keep), collapse = ", "),
                 DBI::dbQuoteIdentifier(con, hit[[1L]]))
  out <- tibble::as_tibble(DBI::dbGetQuery(con, sql))
  if (all(is_blank(out$document_content))) return(NULL)
  out
}

#' Each paper's text, keyed like the records
#'
#' The documents are keyed the way the AI records are, so a cell's paper finds
#' its text: on the same columns when the documents table has them, otherwise
#' on the same kind of identifier.
#'
#' @param documents A table from [read_ecoextract_texts()].
#' @param key The AI records' paper key.
#' @return A tibble with `.paper`, `text`, and `reasoning` (the extraction's
#'   and refinement's reasoning, joined; `NA` when there is none), one row per
#'   paper.
#' @export
document_texts <- function(documents, key = NULL) {
  empty <- empty_tbl(.paper = character(), text = character(),
                     reasoning = character())
  if (is.null(documents) || !NROW(documents)) return(empty)
  doc_key <- if (!is.null(key) && all(key$columns %in% names(documents))) {
    key
  } else {
    choose_paper_keys(list(ai = documents),
                      strategy = if (!is.null(key)) key$strategy)$ai
  }
  if (is.null(doc_key)) return(empty)
  reasons <- intersect(c("extraction_reasoning", "refinement_reasoning"),
                       names(documents))
  reasoning <- if (length(reasons)) {
    apply(as.data.frame(documents[reasons]), 1, function(r) {
      r <- r[!is_blank(r)]
      if (length(r)) paste(r, collapse = "\n\n") else NA_character_
    })
  } else NA_character_
  out <- tibble::tibble(
    .paper = paper_key_values(documents, doc_key),
    text = as.character(documents$document_content),
    reasoning = as.character(reasoning)
  )
  out <- out[!is.na(out$.paper), , drop = FALSE]
  dplyr::distinct(out, .data$.paper, .keep_all = TRUE)
}

#' A search pattern for a value in OCR text
#'
#' Case-insensitive and loose about whitespace, since OCR breaks lines where
#' the page did. A short value -- a code such as "W" -- must stand alone as a
#' word, or it would match inside every other word on the page.
#'
#' @keywords internal
#' @noRd
term_pattern <- function(term) {
  term <- trimws(gsub("\\s+", " ", term))
  esc <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", term)
  esc <- gsub(" ", "\\s+", esc, fixed = TRUE)
  if (nchar(term) <= 3L || grepl("^[[:alnum:]]", term)) {
    esc <- paste0("(?<![[:alnum:]])", esc, "(?![[:alnum:]])")
  }
  paste0("(?i)", esc)
}

#' Where a cell's values and quotes appear in a paper's text
#'
#' Finds each term in the text and returns the passages around the matches,
#' merged where they overlap. A quoted sentence that does not appear whole --
#' OCR rarely reproduces a sentence exactly -- is looked for by its first
#' words instead.
#'
#' @param text A paper's text.
#' @param terms A named character vector of what to look for. The names say
#'   which side each term belongs to (`"ai"`, `"gold"`, ...) and are carried
#'   through to the marks. Blank terms are skipped.
#' @param window Characters of context either side of a match.
#' @param max_per_term Passages to keep per term.
#'
#' @return A list with `passages` -- a list of passages, each a list of `text`
#'   (the excerpt), `start` (its position in the full text), and `marks` (a
#'   tibble of `start`, `end`, `side`, positions within the excerpt) -- and
#'   `found`, a named logical vector saying which terms were found.
#' @examples
#' p <- document_passages("The bats roosted in a barn in Ohio.",
#'                        c(ai = "barn", gold = "Ohio"))
#' p$passages[[1]]$marks
#' @export
document_passages <- function(text, terms, window = 240L, max_per_term = 3L) {
  terms <- terms[!is_blank(terms)]
  found <- stats::setNames(rep(FALSE, length(terms)), names(terms))
  if (is.null(text) || is.na(text) || !nzchar(text) || !length(terms)) {
    return(list(passages = list(), found = found))
  }

  hits <- list()
  for (i in seq_along(terms)) {
    term <- as.character(terms[[i]])
    m <- gregexpr(term_pattern(term), text, perl = TRUE)[[1L]]
    if (m[[1L]] == -1L && nchar(term) > 80L) {
      # A quoted sentence the OCR did not reproduce whole: its opening words.
      lead <- paste(utils::head(strsplit(trimws(term), "\\s+")[[1L]], 8L),
                    collapse = " ")
      m <- gregexpr(term_pattern(lead), text, perl = TRUE)[[1L]]
    }
    if (m[[1L]] == -1L) next
    found[[i]] <- TRUE
    keep <- utils::head(seq_along(m), max_per_term)
    hits[[length(hits) + 1L]] <- tibble::tibble(
      start = as.integer(m[keep]),
      end = as.integer(m[keep] + attr(m, "match.length")[keep] - 1L),
      side = names(terms)[[i]] %||% ""
    )
  }
  if (!length(hits)) return(list(passages = list(), found = found))
  hits <- dplyr::arrange(dplyr::bind_rows(hits), .data$start)

  # Windows around every match, merged where they touch.
  n <- nchar(text)
  from <- pmax(1L, hits$start - window)
  to <- pmin(n, hits$end + window)
  group <- cumsum(c(TRUE, from[-1L] > cummax(to)[-length(to)]))
  passages <- lapply(split(seq_len(nrow(hits)), group), function(idx) {
    a <- min(from[idx])
    b <- max(to[idx])
    marks <- hits[idx, , drop = FALSE]
    marks$start <- marks$start - a + 1L
    marks$end <- marks$end - a + 1L
    list(text = substr(text, a, b), start = a, marks = marks)
  })
  list(passages = unname(passages), found = found)
}
