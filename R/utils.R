#' Null-coalescing operator
#'
#' Returns `y` when `x` is `NULL` or length zero, otherwise `x`.
#'
#' @param x,y Values to coalesce.
#' @return `x` when it is non-`NULL` and non-empty, otherwise `y`.
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

#' Is a value blank?
#'
#' A cell counts as blank when it is `NA`, an empty string, whitespace only, or
#' a zero-length vector. Blank cells are excluded from precision and recall --
#' see the metrics accounting in `DESIGN.md`.
#'
#' @param x A vector of values.
#' @return A logical vector the same length as `x`.
#' @export
is_blank <- function(x) {
  if (is.null(x)) return(logical(0))
  if (is.list(x)) {
    return(vapply(x, function(el) {
      length(el) == 0L || all(is.na(el)) ||
        (is.character(el) && all(!nzchar(trimws(el))))
    }, logical(1)))
  }
  if (length(x) == 0L) return(logical(0))
  blank <- is.na(x)
  if (is.character(x)) blank <- blank | !nzchar(trimws(x))
  blank
}

#' Coerce a value to a length-one character scalar for display
#'
#' @param x A value.
#' @param collapse Separator used when `x` has several elements.
#' @return A character scalar; `NA_character_` when `x` is blank.
#' @keywords internal
#' @noRd
as_scalar_chr <- function(x, collapse = "; ") {
  if (is.null(x)) return(NA_character_)
  if (is.list(x) && length(x) == 1L) x <- x[[1L]]
  if (length(x) == 0L) return(NA_character_)
  if (all(is.na(x))) return(NA_character_)
  paste(as.character(x[!is.na(x)]), collapse = collapse)
}

#' Split a delimited string into a character vector
#'
#' Array-typed fields arrive either as a list column or as a delimited string.
#' This normalises both to a character vector.
#'
#' @param x A single value: a list element, character scalar, or vector.
#' @param split Regular expression separating elements of a delimited string.
#' @return A character vector with blanks dropped.
#' @keywords internal
#' @noRd
as_set <- function(x, split = "\\s*[;|]\\s*|,\\s+") {
  if (is.list(x) && length(x) == 1L) x <- x[[1L]]
  if (is.null(x) || length(x) == 0L) return(character(0))
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(character(0))
  x <- as.character(x)
  if (length(x) == 1L && grepl(split, x)) x <- strsplit(x, split)[[1L]]
  x <- trimws(x)
  x[nzchar(x)]
}

#' Standardise a column name for fuzzy matching
#'
#' Lower-cases and strips everything but alphanumerics, so `"Bat Species"`,
#' `"bat_species"`, and `"batSpecies"` all collapse to `"batspecies"`.
#'
#' @param x A character vector of names.
#' @return A character vector of squashed names.
#' @keywords internal
#' @noRd
squash_name <- function(x) {
  gsub("[^a-z0-9]", "", tolower(as.character(x)))
}

#' Stable identifier for a value pair
#'
#' Used as the cache key for LLM judge verdicts, which must be frozen into
#' `run_config.json` for a run to be reproducible. Vectorised, so it also
#' serves for counting the distinct pairs a batch judge run would cost.
#'
#' @param field Field name.
#' @param a,b The two values being compared, as character scalars or vectors.
#' @return A character vector of keys.
#' @keywords internal
#' @noRd
pair_key <- function(field, a, b) {
  scalarise <- function(x) {
    if (is.list(x)) vapply(x, as_scalar_chr, character(1)) else as.character(x)
  }
  paste(field, scalarise(a), scalarise(b), sep = " <|> ")
}

#' Abort with an ecoeval-classed condition
#'
#' @param msg Message text.
#' @param class Extra condition subclass.
#' @return Called for its side effect; never returns.
#' @keywords internal
#' @noRd
eco_abort <- function(msg, class = NULL) {
  rlang::abort(msg, class = c(class, "ecoeval_error"))
}

#' Empty tibble with a given column specification
#'
#' @param ... Named prototype vectors.
#' @return A zero-row tibble.
#' @keywords internal
#' @noRd
empty_tbl <- function(...) {
  tibble::as_tibble(lapply(list(...), function(x) x[0L]))
}

#' What a side actually said, for display
#'
#' Cells carry the value compared (`ai_value`) and, when a normaliser changed
#' it, the value as read (`ai_original`). A cell table from before that column
#' existed has only the first, which is then also the second.
#'
#' @param cells A cell tibble.
#' @param side `"ai"` or `"gold"`.
#' @return A character vector, one per row.
#' @keywords internal
#' @noRd
original_values <- function(cells, side) {
  orig <- cells[[paste0(side, "_original")]]
  value <- cells[[paste0(side, "_value")]]
  if (is.null(orig)) value else orig
}

#' Run an expression with a package's console chatter suppressed
#'
#' Some packages report progress with `cat()`, which `suppressMessages()` does
#' not catch and which would otherwise flood a Shiny console.
#'
#' @param f A function of no arguments.
#' @return The value of `f()`.
#' @keywords internal
#' @noRd
quietly <- function(f) {
  out <- NULL
  utils::capture.output(
    suppressWarnings(suppressMessages(out <- f())),
    file = nullfile()
  )
  out
}
