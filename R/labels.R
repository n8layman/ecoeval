# What the two sides are called.
#
# "Extraction" and "Reference" are the defaults because they claim no more than
# they know: the extraction may be a rules-based parser or an earlier pipeline
# version rather than an AI, and the reference may be an independently filed
# record that can itself be wrong -- calling that side "gold" tells a reviewer
# it is correct when it is only a second source. Name the sides for what they
# are when that is known.
#
# The labels are wording only. Scoring is unchanged: the accounting still
# treats the reference as the side the extraction is measured against, which
# is what precision and recall mean. Neutral mode drops the TP/FP/FN wording
# from the outcome labels, for when the reference is not authoritative and a
# difference is not necessarily the extraction's error.

#' Names for the two sides of a comparison
#'
#' Every plot, outcome label, findings table, and export README that names a
#' side uses these. Pass the result as `labels` to [run_eval_app()],
#' [evaluate_extraction()], or a plotting function, or set it for a session
#' with [use_side_labels()].
#'
#' @param ai What to call the extraction side.
#' @param gold What to call the reference side.
#' @param neutral When `TRUE`, outcome labels drop the TP/FP/FN wording, for a
#'   reference that is a second source rather than ground truth. The metrics
#'   are unchanged.
#'
#' @return An `ecoeval_labels` object.
#' @examples
#' side_labels()
#' side_labels("AI", "Gold standard")
#' side_labels("Parser v2", "Parser v1", neutral = TRUE)
#' @export
side_labels <- function(ai = "Extraction", gold = "Reference", neutral = FALSE) {
  for (x in list(ai, gold)) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
      eco_abort("Side labels are single, non-empty strings.")
    }
  }
  structure(list(ai = ai, gold = gold, neutral = isTRUE(neutral)),
            class = "ecoeval_labels")
}

#' @export
print.ecoeval_labels <- function(x, ...) {
  cat(sprintf("<side labels> %s vs %s%s\n", x$ai, x$gold,
              if (x$neutral) " (neutral wording)" else ""))
  invisible(x)
}

#' Coerce what a caller supplied into side labels
#'
#' @param x `NULL` for the session's labels, an `ecoeval_labels`, or a named
#'   character vector or list with `ai` and/or `gold` (and optionally
#'   `neutral`).
#' @return An `ecoeval_labels`.
#' @keywords internal
#' @noRd
as_side_labels <- function(x) {
  if (is.null(x)) return(current_labels())
  if (inherits(x, "ecoeval_labels")) return(x)
  x <- as.list(x)
  bad <- setdiff(names(x), c("ai", "gold", "neutral"))
  if (is.null(names(x)) || length(bad)) {
    eco_abort("`labels` is side_labels(), or c(ai = \"...\", gold = \"...\").")
  }
  side_labels(ai = x$ai %||% "Extraction", gold = x$gold %||% "Reference",
              neutral = isTRUE(as.logical(x$neutral %||% FALSE)))
}

#' The side labels in force
#'
#' @return The labels set with [use_side_labels()], or the defaults.
#' @export
current_labels <- function() {
  getOption("ecoeval.labels") %||% side_labels()
}

#' Set the side labels for the session
#'
#' Every function that takes `labels` defaults to these.
#'
#' @param labels An `ecoeval_labels`, a named vector as [side_labels()]
#'   accepts, or `NULL` to restore the defaults.
#' @return The previous labels, invisibly, so they can be restored.
#' @export
use_side_labels <- function(labels) {
  old <- getOption("ecoeval.labels")
  options(ecoeval.labels = if (is.null(labels)) NULL else as_side_labels(labels))
  invisible(old)
}

#' "<side> only", the one phrasing that reads for any name
#' @keywords internal
#' @noRd
side_only <- function(labels, side) paste(labels[[side]], "only")

#' A confusion-matrix tag, unless the wording is neutral
#' @keywords internal
#' @noRd
cm_tag <- function(labels, tag) if (labels$neutral) "" else paste0(" (", tag, ")")

#' How a record's kind reads under the side labels
#'
#' [record_field_outcomes()] keeps `kind` as a stable code -- `"matched"`,
#' `"AI only"`, `"gold only"` -- so code can test it; this is what a reader is
#' shown.
#'
#' @param kind A character vector of kinds.
#' @param labels Side labels; see [side_labels()].
#' @return A character vector the same length.
#' @examples
#' record_kind_label(c("matched", "gold only"), side_labels(gold = "Gold standard"))
#' @export
record_kind_label <- function(kind, labels = current_labels()) {
  labels <- as_side_labels(labels)
  out <- as.character(kind)
  out[out %in% "AI only"] <- side_only(labels, "ai")
  out[out %in% "gold only"] <- side_only(labels, "gold")
  out
}
