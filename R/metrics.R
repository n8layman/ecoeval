# The metrics engine.
#
# One unified accounting, computed over the entire set -- every record from
# both sides, matched or not. Per column:
#
#   agree (both populated)  TP        disagree        FP + FN
#   AI populated, gold not  FP        gold only       FN
#   gold populated, AI not  FN        AI only         FP
#   both blank              TN (drops out of precision and recall)
#
# Counting a disagreement as both is the standard multi-class treatment: a
# misclassification is a false positive for the value asserted and a false
# negative for the value missed. It has a useful consequence -- a wrongly
# linked pair that disagrees on everything scores exactly the same as leaving
# those rows unlinked, so bad links cannot inflate field accuracy.
#
# Plain functions over data frames. No Shiny.

#' How each cell state contributes to the accounting
#'
#' @return A tibble with `state`, `tp`, `fp`, `fn`, `tn`.
#' @export
state_contributions <- function() {
  tibble::tribble(
    ~state,          ~tp, ~fp, ~fn, ~tn,
    "agree",           1L,  0L,  0L,  0L,
    "disagree",        0L,  1L,  1L,  0L,
    "ai_missing",      0L,  0L,  1L,  0L,
    "gold_missing",    0L,  1L,  0L,  0L,
    "ai_only",         0L,  1L,  0L,  0L,
    "gold_only",       0L,  0L,  1L,  0L,
    "blank",           0L,  0L,  0L,  1L
  )
}

#' Grid colour for each cell state
#'
#' Fill carries agreement. Schema violations are marked, not coloured --
#' validity is orthogonal to match state, and a cell can be both.
#'
#' @param state A character vector of cell states.
#' @return A character vector of colour names.
#' @export
state_colour <- function(state) {
  c(agree = "green", disagree = "yellow", ai_missing = "yellow",
    gold_missing = "yellow", ai_only = "purple", gold_only = "orange",
    blank = "blank")[state]
}

#' Precision, recall and F1 from raw counts
#'
#' @param tp,fp,fn Counts.
#' @return A tibble with `precision`, `recall`, `f1`.
#' @keywords internal
#' @noRd
prf <- function(tp, fp, fn) {
  precision <- ifelse(tp + fp > 0, tp / (tp + fp), NA_real_)
  recall <- ifelse(tp + fn > 0, tp / (tp + fn), NA_real_)
  f1 <- ifelse(!is.na(precision) & !is.na(recall) & (precision + recall) > 0,
               2 * precision * recall / (precision + recall), NA_real_)
  tibble::tibble(precision = precision, recall = recall, f1 = f1)
}

#' Field-level metrics
#'
#' One row per column: the confusion counts and the metrics derived from them.
#' `accuracy` is over *scored* cells -- those where at least one side had a
#' value. Cells blank on both sides are true negatives and drop out, because a
#' field neither party filled in says nothing about extraction quality.
#'
#' @param cells A cell tibble from [score_cells()].
#' @return A tibble with `field`, `tp`, `fp`, `fn`, `tn`, `n_scored`,
#'   `accuracy`, `precision`, `recall`, `f1`, `n_pending`, `n_overridden`,
#'   sorted worst accuracy first -- the triage order.
#' @export
field_metrics <- function(cells) {
  if (!nrow(cells)) {
    return(empty_tbl(field = character(), tp = integer(), fp = integer(),
                     fn = integer(), tn = integer(), n_scored = integer(),
                     accuracy = numeric(), precision = numeric(),
                     recall = numeric(), f1 = numeric(),
                     n_pending = integer(), n_overridden = integer()))
  }
  contrib <- state_contributions()
  joined <- dplyr::left_join(cells, contrib, by = "state")
  out <- dplyr::summarise(
    dplyr::group_by(joined, .data$field),
    tp = sum(.data$tp), fp = sum(.data$fp),
    fn = sum(.data$fn), tn = sum(.data$tn),
    # Every cell where at least one side had a value; both-blank drops out.
    n_scored = sum(.data$state != "blank"),
    n_pending = sum(.data$pending),
    n_overridden = sum(.data$overridden),
    .groups = "drop"
  )
  out$accuracy <- ifelse(out$n_scored > 0, out$tp / out$n_scored, NA_real_)
  out <- dplyr::bind_cols(out, prf(out$tp, out$fp, out$fn))
  out <- out[, c("field", "tp", "fp", "fn", "tn", "n_scored", "accuracy",
                 "precision", "recall", "f1", "n_pending", "n_overridden")]
  dplyr::arrange(out, .data$accuracy, dplyr::desc(.data$n_scored))
}

#' Record-level metrics
#'
#' Did the AI find the right rows? Matching is 1:1 and leftovers are false
#' positives and false negatives, because that is what they are.
#'
#' Unlike the field metrics, these are **not** robust to alignment error --
#' pairing two records converts 1 FP + 1 FN into 1 TP, which is exactly the
#' judgment the human review exists for.
#'
#' @param pairs A pair tibble from [align_records()].
#' @return A one-row tibble with `tp`, `fp`, `fn`, `precision`, `recall`, `f1`.
#' @export
record_metrics <- function(pairs) {
  tp <- sum(pairs$kind == "pair")
  fp <- sum(pairs$kind == "ai_only")
  fn <- sum(pairs$kind == "gold_only")
  dplyr::bind_cols(tibble::tibble(tp = tp, fp = fp, fn = fn), prf(tp, fp, fn))
}

#' The two aggregate numbers
#'
#' They diverge when fill rates are uneven, and the gap is itself informative.
#'
#' @param cells A cell tibble from [score_cells()].
#' @return A one-row tibble with `overall_accuracy` (every scored cell weighed
#'   the same), `column_mean_accuracy` (every column weighed the same),
#'   `n_scored`, `n_columns`.
#' @export
aggregate_metrics <- function(cells) {
  fm <- field_metrics(cells)
  scored <- sum(fm$n_scored)
  tibble::tibble(
    overall_accuracy = if (scored > 0) sum(fm$tp) / scored else NA_real_,
    column_mean_accuracy = if (nrow(fm)) mean(fm$accuracy, na.rm = TRUE) else NA_real_,
    n_scored = scored,
    n_columns = nrow(fm)
  )
}

#' Per-column confusion matrix
#'
#' The form adapts to the column's type, because a K-by-K matrix over thousands
#' of species names is useless:
#'
#' * **enum / boolean / low cardinality** -- a true K-by-K, gold class by AI
#'   class, with an explicit "not in schema" row and column so out-of-enum
#'   values appear rather than being silently dropped. Those are among the most
#'   interesting cells on the chart.
#' * **free text / high cardinality** -- 2-by-2 on presence and absence, with
#'   the populated-by-both cell split into "correct" and "wrong value". That
#'   split separates *didn't populate the field* from *populated it wrong*,
#'   which have entirely different causes.
#' * **numeric / date** -- presence and absence, plus the distribution of
#'   signed error over the pairs where both sides had a number.
#'
#' @param cells A cell tibble from [score_cells()].
#' @param field The column to describe.
#' @param schema An `ecoeval_schema`, or `NULL`.
#' @param max_k Cardinality above which a column is treated as free text.
#'
#' @return A list with `field`, `type` (`"class"`, `"presence"`, or
#'   `"numeric"`), `matrix` (a long tibble of `gold_class`, `ai_class`, `n`),
#'   `errors` (numeric columns only), and `notes` -- a character vector of
#'   cautions, such as a single class dominating or too few matched records.
#' @export
column_confusion <- function(cells, field, schema = NULL, max_k = 12L) {
  sub <- cells[cells$field == field, , drop = FALSE]
  enum <- schema_enum(schema, field)
  sch_type <- schema_type_of(schema, field)

  gold_vals <- sub$gold_value
  ai_vals <- sub$ai_value
  k <- length(unique(c(gold_vals[!is_blank(gold_vals)], ai_vals[!is_blank(ai_vals)])))

  notes <- character(0)
  n_pairs <- sum(!is.na(sub$ai_rid) & !is.na(sub$gold_rid))
  if (n_pairs < 5L) {
    notes <- c(notes, sprintf("Only %d matched records -- too few to read much into.", n_pairs))
  }

  if (length(enum) || (k > 0L && k <= max_k && !sch_type %in% c("number", "integer"))) {
    m <- class_matrix(sub, enum)
    dom <- dplyr::summarise(dplyr::group_by(m, .data$gold_class),
                            n = sum(.data$n), .groups = "drop")
    if (nrow(dom) && sum(dom$n) > 0 && max(dom$n) / sum(dom$n) > 0.9) {
      notes <- c(notes, sprintf(
        "One class ('%s') is %.0f%% of the gold values -- accuracy here is trivially high.",
        dom$gold_class[which.max(dom$n)], 100 * max(dom$n) / sum(dom$n)
      ))
    }
    return(list(field = field, type = "class", matrix = m, errors = NULL, notes = notes))
  }

  m <- presence_matrix(sub)
  errors <- NULL
  if (sch_type %in% c("number", "integer")) {
    both <- sub[!is_blank(sub$ai_value) & !is_blank(sub$gold_value), , drop = FALSE]
    na <- suppressWarnings(as.numeric(both$ai_value))
    ng <- suppressWarnings(as.numeric(both$gold_value))
    ok <- !is.na(na) & !is.na(ng)
    errors <- tibble::tibble(
      pair_id = both$pair_id[ok],
      gold = ng[ok], ai = na[ok], error = na[ok] - ng[ok]
    )
    return(list(field = field, type = "numeric", matrix = m, errors = errors,
                notes = notes))
  }
  list(field = field, type = "presence", matrix = m, errors = NULL, notes = notes)
}

#' K-by-K class matrix with explicit blank and out-of-schema classes
#'
#' @param sub Cells for one field.
#' @param enum The field's enum, or an empty vector.
#' @return A long tibble of `gold_class`, `ai_class`, `n`.
#' @keywords internal
#' @noRd
class_matrix <- function(sub, enum = character(0)) {
  classify <- function(x) {
    out <- canonicalise(x)
    out[is_blank(x)] <- "(blank)"
    if (length(enum)) {
      known <- canonicalise(enum)
      idx <- match(out, known)
      out <- ifelse(!is.na(idx), enum[idx],
                    ifelse(out == "(blank)", "(blank)", "(not in schema)"))
    }
    out
  }
  g <- classify(sub$gold_value)
  a <- classify(sub$ai_value)
  g[is.na(sub$gold_rid)] <- "(no gold record)"
  a[is.na(sub$ai_rid)] <- "(no AI record)"
  # Each axis gets only the classes that can occur on it: "(no AI record)" is
  # never an AI class, and an empty row for it would be noise on the chart.
  base <- unique(c(if (length(enum)) enum else character(0),
                   setdiff(c(g, a), c("(no gold record)", "(no AI record)"))))
  gold_levels <- c(base, if ("(no gold record)" %in% g) "(no gold record)")
  ai_levels <- c(base, if ("(no AI record)" %in% a) "(no AI record)")
  grid <- expand.grid(gold_class = gold_levels, ai_class = ai_levels,
                      stringsAsFactors = FALSE)
  counts <- dplyr::count(tibble::tibble(gold_class = g, ai_class = a),
                         .data$gold_class, .data$ai_class, name = "n")
  out <- dplyr::left_join(tibble::as_tibble(grid), counts,
                          by = c("gold_class", "ai_class"))
  out$n <- dplyr::coalesce(out$n, 0L)
  out
}

#' Presence-absence matrix with the populated-by-both cell split
#'
#' @param sub Cells for one field.
#' @return A long tibble of `gold_class`, `ai_class`, `n`.
#' @keywords internal
#' @noRd
presence_matrix <- function(sub) {
  has_g <- !is_blank(sub$gold_value)
  has_a <- !is_blank(sub$ai_value)
  agree <- sub$state == "agree"
  cls <- ifelse(
    has_g & has_a & agree, "both: correct",
    ifelse(has_g & has_a, "both: wrong value",
           ifelse(has_g, "gold only", ifelse(has_a, "AI only", "neither")))
  )
  levels_all <- c("both: correct", "both: wrong value", "gold only",
                  "AI only", "neither")
  counts <- dplyr::count(tibble::tibble(cell = factor(cls, levels_all)),
                         .data$cell, .drop = FALSE, name = "n")
  tibble::tibble(
    gold_class = c("populated", "populated", "populated", "blank", "blank"),
    ai_class = c("correct", "wrong value", "blank", "populated", "blank"),
    n = as.integer(counts$n[match(levels_all, as.character(counts$cell))])
  )
}

#' @keywords internal
#' @noRd
schema_enum <- function(schema, field) {
  if (is.null(schema)) return(character(0))
  i <- match(field, schema$fields$field)
  if (is.na(i)) return(character(0))
  schema$fields$enum[[i]]
}

#' @keywords internal
#' @noRd
schema_type_of <- function(schema, field) {
  if (is.null(schema)) return("string")
  i <- match(field, schema$fields$field)
  if (is.na(i)) return("string")
  schema$fields$type[[i]]
}

#' Fill rates for both sources, side by side
#'
#' Feeds the completeness chart and the fill-rate asymmetry finding: a field
#' humans always populate and the AI rarely does is usually a field-description
#' problem, not a model failure.
#'
#' @param cells A cell tibble from [score_cells()].
#' @return A tibble with `field`, `ai_fill`, `gold_fill`, `gap`, sorted by the
#'   largest gap first.
#' @export
fill_rates <- function(cells) {
  if (!nrow(cells)) {
    return(empty_tbl(field = character(), ai_fill = numeric(),
                     gold_fill = numeric(), gap = numeric()))
  }
  rate <- function(value, present) {
    v <- value[present]
    if (!length(v)) NA_real_ else mean(!is_blank(v))
  }
  out <- dplyr::summarise(
    dplyr::group_by(cells, .data$field),
    ai_fill = rate(.data$ai_value, !is.na(.data$ai_rid)),
    gold_fill = rate(.data$gold_value, !is.na(.data$gold_rid)),
    .groups = "drop"
  )
  out$gap <- out$gold_fill - out$ai_fill
  dplyr::arrange(out, dplyr::desc(abs(.data$gap)))
}

#' Review progress, in the three states that matter
#'
#' Distinguishing them is honest about how the numbers were produced:
#' *unjudged* means only the cheap comparator rungs ran, *judged* means the LLM
#' settled the contested cells, *reviewed* means a person looked at it.
#'
#' @param cells A cell tibble from [score_cells()].
#' @param papers Character vector of papers in scope.
#' @param reviewed Character vector of papers a person has opened and reviewed.
#' @return A one-row tibble with `n_papers`, `n_judged`, `n_reviewed`,
#'   `n_pending_cells`.
#' @export
progress_summary <- function(cells, papers, reviewed = character(0)) {
  pending_by_paper <- if (nrow(cells)) {
    tapply(cells$pending, cells$paper, any)
  } else structure(logical(0), names = character(0))
  judged <- setdiff(papers, names(pending_by_paper)[which(pending_by_paper)])
  tibble::tibble(
    n_papers = length(papers),
    n_judged = length(judged),
    n_reviewed = length(intersect(reviewed, papers)),
    n_pending_cells = if (nrow(cells)) sum(cells$pending) else 0L
  )
}

#' Pairings that look wrong
#'
#' Two cheap signals, both strong. A pair that agrees on **nothing** is almost
#' certainly two unrelated records the matcher joined. A pair that disagrees on
#' every **identity column** is the subtler case: it may agree on incidentals
#' like country and year while the fields that establish which record this
#' actually is all disagree.
#'
#' This is what turns "review all 47 papers or none" into "look at these
#' three", and it is the bridge between the fast path and the review path.
#'
#' @param cells A cell tibble from [score_cells()].
#' @param linkage_fields The identity columns, from the comparator config.
#' @return A tibble with `pair_id`, `paper`, `n_fields`, `reason`, sorted by
#'   paper.
#' @export
suspect_pairs <- function(cells, linkage_fields = character(0)) {
  paired <- cells[!is.na(cells$ai_rid) & !is.na(cells$gold_rid) &
                    cells$state != "blank", , drop = FALSE]
  proto <- empty_tbl(pair_id = character(), paper = character(),
                     n_fields = integer(), reason = character())
  if (!nrow(paired)) return(proto)

  linkage_fields <- intersect(linkage_fields, unique(paired$field))
  out <- dplyr::summarise(
    dplyr::group_by(paired, .data$pair_id, .data$paper),
    n_fields = dplyr::n(),
    n_agree = sum(.data$state == "agree"),
    n_id = sum(.data$field %in% linkage_fields),
    n_id_agree = sum(.data$field %in% linkage_fields & .data$state == "agree"),
    .groups = "drop"
  )
  out$reason <- ifelse(
    out$n_agree == 0L & out$n_fields > 1L, "agrees on nothing",
    ifelse(out$n_id > 0L & out$n_id_agree == 0L,
           "identity columns all disagree", NA_character_)
  )
  out <- out[!is.na(out$reason), c("pair_id", "paper", "n_fields", "reason")]
  dplyr::arrange(out, .data$paper)
}
