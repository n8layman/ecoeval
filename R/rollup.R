# Rolling the extraction up to the reference's grain.
#
# An extraction can record a paper in more detail than the reference: twelve
# rows by size class where the reference has one row with the total. Matched
# one to one, eleven of the twelve become extraction-only records. Rolling the
# extraction up -- grouping its records within each paper and summing the
# quantities -- puts both sides at the same grain before they are matched.
#
# Only the extraction is ever rolled up. The grouping rules are ecoreview's, so
# a table rolled up in ecoreview and in ecoeval comes out the same.

#' Roll up records by key columns
#'
#' Copied from `ecoreview::rollup_records()` (ecoreview 0.1.59) so both
#' packages roll up identically without ecoeval depending on ecoreview. When
#' ecoreview's version changes, copy it again.
#'
#' Groups each document's records by `by` and sums the `sum` columns. `by`
#' columns hold the group's values; each `sum` column holds the members' total
#' (`NA` only when every member is `NA`) and is followed by `<col>_values`, a
#' JSON array of every member's value; every other column holds the members'
#' distinct non-missing values as a JSON array, with JSON-array values merged
#' element by element. `n_records` is added last. Attribute `"member_rows"`
#' gives each group's member rows in `records`.
#'
#' @param records Data frame of records.
#' @param by Columns that define a group within a document.
#' @param sum Numeric columns to sum within a group.
#' @return A data frame with one row per group.
#' @keywords internal
#' @noRd
rollup_records <- function(records, by, sum = character(0)) {
  by <- as.character(by)
  sum <- setdiff(as.character(sum), by)
  missing_cols <- setdiff(c(by, sum), names(records))
  if (length(missing_cols) > 0) {
    stop("Columns not found in records: ", paste(missing_cols, collapse = ", "),
         call. = FALSE)
  }
  non_numeric <- sum[!vapply(records[sum], is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop("Columns to sum must be numeric: ", paste(non_numeric, collapse = ", "),
         call. = FALSE)
  }

  records <- as.data.frame(records, stringsAsFactors = FALSE)
  row_nums <- seq_len(nrow(records))
  if ("deleted_by_user" %in% names(records)) {
    row_nums <- row_nums[is.na(records$deleted_by_user)]
  }
  live <- records[row_nums, , drop = FALSE]

  keys <- c(intersect("document_id", names(live)), by)
  key_strings <- if (nrow(live) > 0) {
    do.call(paste, c(lapply(live[keys], function(x) {
      ifelse(is.na(x), "\u0001NA", as.character(x))
    }), sep = "\u0002"))
  } else character(0)
  groups <- unname(split(seq_len(nrow(live)), factor(key_strings, levels = unique(key_strings))))
  first_rows <- vapply(groups, `[`, integer(1), 1L)

  in_cols <- setdiff(names(live), c("id", "deleted_by_user"))
  out <- live[first_rows, keys, drop = FALSE]
  for (col in setdiff(in_cols, keys)) {
    x <- live[[col]]
    if (col %in% sum) {
      out[[col]] <- vapply(groups, function(g) {
        if (all(is.na(x[g]))) NA_real_ else base::sum(x[g], na.rm = TRUE)
      }, numeric(1))
      out[[paste0(col, "_values")]] <- vapply(groups, function(g) {
        as.character(jsonlite::toJSON(x[g], na = "null", digits = NA))
      }, character(1))
    } else {
      out[[col]] <- vapply(groups, function(g) .rollup_values(x[g]), character(1))
    }
  }
  out$n_records <- lengths(groups)
  out <- out[, c(unlist(lapply(in_cols, function(col) {
    if (col %in% sum) c(col, paste0(col, "_values")) else col
  })), "n_records"), drop = FALSE]

  rownames(out) <- NULL
  attr(out, "member_rows") <- lapply(groups, function(g) row_nums[g])
  out
}

# Members' distinct non-missing values as a JSON array, NA when there are
# none. Character values holding a JSON array of scalars are merged element
# by element. (ecoreview)
.rollup_values <- function(x) {
  x <- x[!is.na(x)]
  if (is.character(x)) {
    x <- unlist(lapply(x, .rollup_json_elements))
    x <- x[!is.na(x) & nzchar(x)]
  }
  x <- unique(x)
  if (length(x) == 0) return(NA_character_)
  as.character(jsonlite::toJSON(x, digits = NA))
}

# Elements of a JSON array of scalars, as strings; the value itself otherwise.
# (ecoreview)
.rollup_json_elements <- function(raw) {
  if (!startsWith(trimws(raw), "[")) return(raw)
  parsed <- tryCatch(jsonlite::fromJSON(raw, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (!is.list(parsed) ||
      !all(vapply(parsed, function(p) is.null(p) || (is.atomic(p) && length(p) == 1L),
                  logical(1)))) {
    return(raw)
  }
  vapply(parsed, function(p) if (is.null(p)) NA_character_ else as.character(p),
         character(1))
}

#' Roll up the extraction's records
#'
#' Groups the extraction's records within each paper by the `by` fields and
#' adds up the `sum` fields, following ecoreview's rules: a total is `NA` only
#' when every member is blank, and every other field keeps each distinct value
#' its members hold. Only the extraction is rolled up; the reference keeps its
#' rows.
#'
#' A field whose members hold more than one value comes back as a list element
#' holding all of them. [score_cells()] counts such a cell as agreeing when the
#' reference value is one of them, and marks it `mixed`. Fields that already
#' held lists (array fields) stay sets.
#'
#' @param records The extraction's records, from [prepare_records()]: `.rid`,
#'   `.paper`, and the field columns.
#' @param by Fields that define a group within a paper.
#' @param sum Numeric fields to add up within a group.
#' @param members Instead of grouping by `by`, the groups to use: a list of
#'   row numbers, as returned in the `"members"` attribute. Used to roll up the
#'   values as read with the same groups as the compared ones.
#' @return A tibble with one row per group: `.rid` (new keys, `ar00001`, ...),
#'   `.paper`, the fields, and `.n_records`. Attribute `"members"` is a list of
#'   each group's member rows in `records`, and `"member_rids"` the same as
#'   `.rid` values.
#' @export
rollup_extraction <- function(records, by, sum = character(0), members = NULL) {
  by <- as.character(by)
  sum <- setdiff(as.character(sum), by)
  fields <- setdiff(names(records), c(".rid", ".paper"))
  missing <- setdiff(c(by, sum), fields)
  if (length(missing)) {
    eco_abort(paste0("Roll-up field(s) not among the compared fields: ",
                     paste(missing, collapse = ", "), "."))
  }
  if (!length(by) && is.null(members)) {
    eco_abort("Name at least one field to roll up by.")
  }
  # Array fields arrive as list columns; as JSON arrays they merge element by
  # element, which keeps them sets.
  sets <- fields[vapply(fields, function(f) is.list(records[[f]]), logical(1))]
  flat <- tibble::tibble(document_id = records$.paper)
  for (f in fields) {
    v <- records[[f]]
    flat[[f]] <- if (f %in% sum) {
      suppressWarnings(as.numeric(vapply(seq_along(v), function(i) {
        as_scalar_chr(if (is.list(v)) v[[i]] else v[[i]])
      }, character(1))))
    } else if (f %in% sets) {
      vapply(v, function(x) {
        x <- x[!is.na(x)]
        if (!length(x)) NA_character_ else as.character(jsonlite::toJSON(as.character(x)))
      }, character(1))
    } else {
      as.character(v)
    }
  }

  if (is.null(members)) {
    rolled <- rollup_records(flat, by, sum)
    members <- attr(rolled, "member_rows")
  } else {
    rolled <- regroup(flat, members, by, sum)
  }

  out <- tibble::tibble(.rid = sprintf("ar%05d", seq_along(members)),
                        .paper = rolled$document_id)
  for (f in fields) {
    if (f %in% c(by, sum)) {
      out[[f]] <- rolled[[f]]
      next
    }
    parsed <- lapply(rolled[[f]], function(x) {
      if (is.na(x)) return(NA_character_)
      v <- unlist(jsonlite::fromJSON(x, simplifyVector = FALSE), use.names = FALSE)
      if (is.null(v)) NA_character_ else as.character(v)
    })
    single <- lengths(parsed) <= 1L
    out[[f]] <- if (f %in% sets || !all(single)) parsed else unlist(parsed)
  }
  out$.n_records <- lengths(members)
  attr(out, "members") <- members
  attr(out, "member_rids") <- lapply(members, function(m) records$.rid[m])
  out
}

# The rolled-up table for groups already decided, with rollup_records()'s
# rules. Lets the values as read be rolled up with the groups the compared,
# normalised values formed, even where normalising changed which rows match.
regroup <- function(flat, members, by, sum) {
  out <- tibble::tibble(
    document_id = vapply(members, function(g) flat$document_id[[g[[1L]]]], character(1))
  )
  for (col in setdiff(names(flat), "document_id")) {
    x <- flat[[col]]
    out[[col]] <- if (col %in% by) {
      vapply(members, function(g) as.character(x[[g[[1L]]]]), character(1))
    } else if (col %in% sum) {
      vapply(members, function(g) {
        if (all(is.na(x[g]))) NA_real_ else base::sum(x[g], na.rm = TRUE)
      }, numeric(1))
    } else {
      vapply(members, function(g) .rollup_values(x[g]), character(1))
    }
  }
  out
}

#' Suggest how to roll up the extraction
#'
#' Tries grouping the extraction by each combination of `fields` and keeps the
#' one that best matches the reference's rows. For each candidate it counts:
#'
#' * `matched`: reference records that exactly one extraction group matches on
#'   the grouping fields (compared after [canonicalise()]), within the same
#'   paper;
#' * `merged`: how many of those groups hold more than one extraction record --
#'   the rows the roll-up would merge;
#' * for each numeric field, how often a merged group's total equals the
#'   reference value.
#'
#' The best candidate pairs the most reference records, then has the most
#' totals that match, then groups by the most fields, since that merges the
#' fewest records. The baseline is the extraction as it is. A roll-up is
#' suggested only when it pairs more reference records than the baseline and
#' merges at least one group. A numeric field is suggested for adding up
#' when the totals match the reference in at least half the merged groups.
#'
#' @param ai,gold Record tibbles with `.paper` and the field columns, after
#'   normalisation.
#' @param fields Candidate fields to group by, usually the identity columns.
#' @param numeric_fields Fields that could be added up.
#' @param paper_map Accepted paper links: `ai_paper`, `gold_paper`.
#' @param max_fields Combinations of more than this many fields are not tried.
#' @return A list: `by` and `sum` (the suggestion, `NULL` when there is
#'   none), `baseline` (reference records matched without a roll-up),
#'   `candidates` (a tibble, best first, with `by`, `matched`, `merged`,
#'   `n_groups`, and a `sum_<field>` match rate per numeric field), and
#'   `summary` (one sentence, or `NULL`).
#' @export
suggest_rollup <- function(ai, gold, fields, numeric_fields = character(0),
                           paper_map = NULL, max_fields = 4L) {
  fields <- intersect(fields, intersect(names(ai), names(gold)))
  numeric_fields <- setdiff(intersect(numeric_fields, intersect(names(ai), names(gold))),
                            fields)
  none <- list(by = NULL, sum = NULL, baseline = 0L,
               candidates = tibble::tibble(), summary = NULL)
  if (!length(fields) || !nrow(ai) || !nrow(gold)) return(none)
  paper_map <- paper_map %||% common_paper_map(ai, gold)
  ai <- ai[ai$.paper %in% paper_map$ai_paper, , drop = FALSE]
  if (!nrow(ai)) return(none)
  # Name the extraction's papers by the reference's, so keys compare.
  ai$.paper <- paper_map$gold_paper[match(ai$.paper, paper_map$ai_paper)]

  key_of <- function(df, by) {
    parts <- lapply(by, function(f) {
      canonicalise(vapply(seq_len(nrow(df)), function(i) as_scalar_chr(cell_value(df, i, f)),
                          character(1)))
    })
    do.call(paste, c(list(df$.paper), parts, sep = "\u0002"))
  }
  as_num <- function(x) suppressWarnings(as.numeric(vapply(x, as_scalar_chr, character(1))))

  evaluate <- function(by) {
    rolled <- rollup_extraction(ai, by, numeric_fields)
    rkey <- key_of(rolled, by)
    gkey <- key_of(gold, by)
    gcount <- table(gkey)
    rcount <- table(rkey)
    # A reference record is matched when its key is unique on both sides.
    hit <- rkey %in% names(gcount)[gcount == 1L] & rkey %in% names(rcount)[rcount == 1L]
    merged <- hit & rolled$.n_records > 1L
    out <- tibble::tibble(by = list(by), matched = sum(hit), merged = sum(merged),
                          n_groups = nrow(rolled))
    for (f in numeric_fields) {
      if (!any(merged)) {
        out[[paste0("sum_", f)]] <- NA_real_
        next
      }
      g <- as_num(gold[[f]])[match(rkey[merged], gkey)]
      r <- suppressWarnings(as.numeric(rolled[[f]][merged]))
      ok <- !is.na(g) & !is.na(r)
      out[[paste0("sum_", f)]] <- if (any(ok)) mean(abs(g[ok] - r[ok]) <= 1e-6) else NA_real_
    }
    out
  }

  baseline <- evaluate(fields)
  baseline_matched <- baseline$matched - baseline$merged
  sizes <- seq_len(min(length(fields), max_fields))
  combos <- unlist(lapply(sizes, function(k) utils::combn(fields, k, simplify = FALSE)),
                   recursive = FALSE)
  cands <- dplyr::bind_rows(lapply(combos, evaluate))
  sum_cols <- grep("^sum_", names(cands), value = TRUE)
  sum_hits <- if (length(sum_cols)) {
    rowSums(as.matrix(cands[sum_cols]) * cands$merged, na.rm = TRUE)
  } else 0
  # On a tie, group by more fields: it merges fewer records.
  cands <- cands[order(-cands$matched, -sum_hits, -lengths(cands$by)), , drop = FALSE]

  best <- cands[1L, ]
  if (best$matched <= baseline_matched || best$merged == 0L) {
    none$baseline <- baseline_matched
    none$candidates <- cands
    return(none)
  }
  by <- best$by[[1L]]
  rates <- if (length(sum_cols)) unlist(best[sum_cols]) else numeric(0)
  sum <- sub("^sum_", "", names(rates)[!is.na(rates) & rates >= 0.5])
  summary <- sprintf(
    "Rolling up by %s pairs %d reference records instead of %d, by merging %d %s.",
    paste(by, collapse = " + "), best$matched, baseline_matched, best$merged,
    if (best$merged == 1L) "group of rows" else "groups of rows")
  for (f in sum) {
    summary <- paste(summary, sprintf(
      "Totals of %s match the reference in %.0f%% of merged groups.",
      f, 100 * rates[[paste0("sum_", f)]]))
  }
  list(by = by, sum = sum, baseline = baseline_matched, candidates = cands,
       summary = summary)
}
