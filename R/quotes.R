# Supporting sentences.
#
# Extraction schemas in this family carry a column holding the sentences a
# record's values were read out of. When the two sides disagree it is what
# says which of them read the paper correctly, so the cell pop-up shows it.

#' Split a supporting-sentence value into its sentences
#'
#' ecoextract stores the sentences a record was read from as a JSON array of
#' strings. Shown as is, that is one run of brackets, quotes, and escaped
#' newlines; split, it is the passages the extraction pointed at, each shown
#' on its own. A value that is not a JSON array is one passage.
#'
#' @param x One value: a JSON array of strings, a character vector or list of
#'   passages, or plain text.
#' @return A character vector of passages, blanks dropped; empty when there
#'   are none.
#' @examples
#' quote_sentences('["Bats roosted in the barn.", "Two were captured."]')
#' quote_sentences("Bats roosted in the barn.")
#' @export
quote_sentences <- function(x) {
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  x <- as.character(x)
  x <- x[!is.na(x)]
  out <- unlist(lapply(x, function(v) {
    s <- trimws(v)
    if (grepl("^\\[", s) && grepl("\\]$", s)) {
      parsed <- tryCatch(jsonlite::fromJSON(s, simplifyVector = TRUE),
                         error = function(e) NULL)
      if (!is.null(parsed)) return(as.character(unlist(parsed, use.names = FALSE)))
    }
    v
  }), use.names = FALSE)
  out <- trimws(as.character(out))
  out[!is.na(out) & nzchar(out)]
}
