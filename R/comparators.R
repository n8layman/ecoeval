# The comparator cascade.
#
# Comparison is a ladder, not a single test: exact -> normalized -> fuzzy ->
# LLM judge. Each rung sees only what the previous could not resolve, so the
# expensive rungs run on a small minority of cells. A field's configured
# comparator names the *highest* rung it is allowed to climb to.
#
# Everything in this file is a plain function over vectors and data frames. No
# Shiny, no side effects beyond the caches passed in explicitly.

#' Comparator rungs, cheapest first
#'
#' @param comparator A comparator name.
#' @return The ordered rungs the cascade will try for that comparator.
#' @keywords internal
#' @noRd
cascade_rungs <- function(comparator) {
  switch(
    comparator,
    exact      = "exact",
    normalized = c("exact", "normalized"),
    numeric    = c("exact", "numeric"),
    date       = c("exact", "date"),
    set        = c("exact", "set"),
    fuzzy      = c("exact", "normalized", "fuzzy"),
    judge      = c("exact", "normalized", "fuzzy", "judge"),
    c("exact", "normalized")
  )
}

#' Comparator names available to the user
#'
#' @return A named character vector: names are labels, values are comparator ids.
#' @export
comparator_choices <- function() {
  c(
    "Exact" = "exact",
    "Trimmed, case-insensitive" = "normalized",
    "Numeric tolerance" = "numeric",
    "Date (parse, then compare)" = "date",
    "Set comparison" = "set",
    "Fuzzy (string distance)" = "fuzzy",
    "LLM judge" = "judge"
  )
}

#' Canonical text form used by the `normalized` rung
#'
#' Trims, case-folds, and squashes internal whitespace. Punctuation is left
#' alone -- stripping it would quietly equate values that differ meaningfully.
#'
#' @param x A character vector.
#' @return A character vector of canonical forms.
#' @export
canonicalise <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("\\s+", " ", x)
  tolower(x)
}

#' Parse dates in whatever format they arrive in
#'
#' Gold standards and extraction output rarely agree on date format, and a
#' column comparing as "always wrong" for that reason is the single most common
#' first-run false alarm.
#'
#' A candidate format is accepted only if the parsed date **round-trips** back
#' to the original text (ignoring zero-padding). `strptime()` ignores trailing
#' characters, so without that check `"05/01/2020"` parses under `"%Y/%m/%d"`
#' as the year 5. Across a vector the format that parses the most elements
#' wins, which is how a column with one house style gets read consistently;
#' ties go to the earlier format, so `DD/MM/YYYY` is preferred over
#' `MM/DD/YYYY` for genuinely ambiguous values.
#'
#' @param x A character vector.
#' @return A `Date` vector, `NA` where nothing parsed.
#' @export
parse_date_loose <- function(x) {
  x <- trimws(as.character(x))
  n <- length(x)
  if (!n) return(as.Date(character(0)))
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y",
            "%d %B %Y", "%d %b %Y", "%B %d, %Y", "%b %d, %Y", "%Y-%m", "%Y")

  attempts <- lapply(fmts, function(fmt) {
    # A partial date has no day (or no month) to take from the string, and
    # as.Date() would silently borrow today's -- pin it to the period start.
    pad <- switch(fmt, "%Y" = "-01-01", "%Y-%m" = "-01", "")
    d <- suppressWarnings(as.Date(paste0(x, pad),
                                  format = paste0(fmt, switch(fmt, "%Y" = "-%m-%d",
                                                              "%Y-%m" = "-%d", ""))))
    ok <- !is.na(d) & undecorate_date(format(d, fmt)) == undecorate_date(x)
    d[!ok] <- NA
    d
  })
  hits <- vapply(attempts, function(d) sum(!is.na(d)), integer(1))
  if (!any(hits > 0L)) return(rep(as.Date(NA), n))

  # Best format first, then let the rest fill in stragglers.
  out <- rep(as.Date(NA), n)
  for (i in order(-hits, seq_along(hits))) {
    todo <- is.na(out)
    if (!any(todo)) break
    out[todo] <- attempts[[i]][todo]
  }
  out
}

#' Strip zero-padding so a re-formatted date can be compared to its source
#'
#' @param x A character vector.
#' @return `x` with leading zeros removed from numeric groups and case folded.
#' @keywords internal
#' @noRd
undecorate_date <- function(x) {
  x <- tolower(trimws(as.character(x)))
  gsub("(^|[^0-9])0+([0-9])", "\\1\\2", x)
}

#' String similarity, scaled to zero through one
#'
#' @param a,b Character vectors, recycled against each other.
#' @param method A `stringdist` method. Jaro-Winkler by default, which is also
#'   what the record matcher scores identity columns with, so thresholds read
#'   consistently across the app.
#' @return A numeric vector of similarities; `NA` where either side is blank.
#' @export
similarity <- function(a, b, method = "jw") {
  a <- canonicalise(a)
  b <- canonicalise(b)
  d <- stringdist::stringdist(a, b, method = method)
  if (method %in% c("osa", "lv", "dl", "lcs", "hamming", "qgram")) {
    denom <- pmax(nchar(a), nchar(b), 1)
    d <- d / denom
  }
  sim <- 1 - d
  sim[is.na(a) | is.na(b)] <- NA_real_
  pmin(pmax(sim, 0), 1)
}

#' Set similarity between two collections
#'
#' @param a,b Values coercible to sets: a list column, or a string
#'   delimited by semicolons, pipes, or commas.
#' @param mode `"exact"` (identical sets), `"jaccard"`, or `"overlap"` (any
#'   shared element).
#' @return A numeric score in `[0, 1]`.
#' @keywords internal
#' @noRd
set_score <- function(a, b, mode = "jaccard") {
  sa <- unique(canonicalise(as_set(a)))
  sb <- unique(canonicalise(as_set(b)))
  if (!length(sa) && !length(sb)) return(1)
  if (!length(sa) || !length(sb)) return(0)
  inter <- length(intersect(sa, sb))
  switch(
    mode,
    exact = as.numeric(setequal(sa, sb)),
    overlap = as.numeric(inter > 0),
    inter / length(union(sa, sb))
  )
}

#' Compare one pair of values through the cascade
#'
#' @param a,b The AI and gold values.
#' @param comparator Highest rung to climb to; see [comparator_choices()].
#' @param threshold Fuzzy / set agreement cutoff.
#' @param tolerance Numeric tolerance.
#' @param set_mode Set comparison mode.
#' @param judge A function `(field, a, b)` returning a list with `agree` and
#'   `rationale`, or `NULL` when no judge is configured.
#' @param field Field name, passed to the judge and used as a cache key.
#' @param cache An environment of cached judge verdicts, keyed by value pair.
#'
#' @return A one-row list with `agree`, `rung`, `score`, `rationale`, `pending`.
#'   `pending` is `TRUE` when the field asked for the judge but no judge was
#'   available -- the verdict shown is the fuzzy one, and the cell counts as
#'   unjudged.
#' @export
compare_pair <- function(a, b,
                         comparator = "normalized",
                         threshold = 0.85,
                         tolerance = 1e-6,
                         set_mode = "jaccard",
                         judge = NULL,
                         field = NA_character_,
                         cache = NULL) {
  blank_a <- isTRUE(is_blank(list(a))[[1L]])
  blank_b <- isTRUE(is_blank(list(b))[[1L]])
  if (blank_a || blank_b) {
    return(cmp_result(agree = blank_a && blank_b, rung = "blank",
                      score = if (blank_a && blank_b) 1 else 0))
  }

  rungs <- cascade_rungs(comparator)
  ca <- as_scalar_chr(a)
  cb <- as_scalar_chr(b)
  best_score <- 0

  for (rung in rungs) {
    res <- switch(
      rung,
      exact = list(agree = identical(ca, cb), score = as.numeric(identical(ca, cb))),
      normalized = {
        ok <- identical(canonicalise(ca), canonicalise(cb))
        list(agree = ok, score = as.numeric(ok))
      },
      numeric = {
        na <- suppressWarnings(as.numeric(ca))
        nb <- suppressWarnings(as.numeric(cb))
        if (is.na(na) || is.na(nb)) list(agree = FALSE, score = 0)
        else list(agree = abs(na - nb) <= tolerance, score = as.numeric(abs(na - nb) <= tolerance))
      },
      date = {
        da <- parse_date_loose(ca)
        db <- parse_date_loose(cb)
        if (is.na(da) || is.na(db)) list(agree = FALSE, score = 0)
        else list(agree = da == db, score = as.numeric(da == db))
      },
      set = {
        s <- set_score(a, b, set_mode)
        cut <- if (set_mode == "exact" || set_mode == "overlap") 1 else threshold
        list(agree = s >= cut, score = s)
      },
      fuzzy = {
        s <- similarity(ca, cb)
        list(agree = !is.na(s) && s >= threshold, score = s %||% 0)
      },
      judge = {
        if (is.null(judge)) {
          return(cmp_result(agree = best_agree_so_far(best_score, threshold),
                            rung = "fuzzy", score = best_score, pending = TRUE))
        }
        v <- judge_cached(judge, field, ca, cb, cache)
        list(agree = isTRUE(v$agree), score = as.numeric(isTRUE(v$agree)),
             rationale = v$rationale %||% NA_character_)
      }
    )
    best_score <- max(best_score, res$score %||% 0, na.rm = TRUE)
    if (isTRUE(res$agree)) {
      return(cmp_result(agree = TRUE, rung = rung, score = res$score,
                        rationale = res$rationale %||% NA_character_))
    }
    if (rung == "judge") {
      return(cmp_result(agree = FALSE, rung = "judge", score = res$score,
                        rationale = res$rationale %||% NA_character_))
    }
  }
  cmp_result(agree = FALSE, rung = utils::tail(rungs, 1L), score = best_score)
}

#' @keywords internal
#' @noRd
best_agree_so_far <- function(score, threshold) isTRUE(score >= threshold)

#' @keywords internal
#' @noRd
cmp_result <- function(agree, rung, score = NA_real_, rationale = NA_character_,
                       pending = FALSE) {
  list(agree = isTRUE(agree), rung = rung, score = as.numeric(score %||% NA_real_),
       rationale = as.character(rationale %||% NA_character_), pending = pending)
}

#' Look a judge verdict up in the cache, calling the judge on a miss
#'
#' LLM output varies between calls, so a reproducible run needs verdicts frozen
#' rather than re-derived. The cache is written out with `run_config.json`.
#'
#' @inheritParams compare_pair
#' @param a,b Canonical string forms of the two values.
#' @return A list with `agree` and `rationale`.
#' @keywords internal
#' @noRd
judge_cached <- function(judge, field, a, b, cache = NULL) {
  key <- pair_key(field, a, b)
  if (!is.null(cache) && !is.null(cache[[key]])) return(cache[[key]])
  v <- tryCatch(judge(field, a, b), error = function(e) {
    list(agree = FALSE, rationale = paste("judge failed:", conditionMessage(e)))
  })
  v <- list(agree = isTRUE(v$agree), rationale = as.character(v$rationale %||% NA_character_))
  if (!is.null(cache)) assign(key, v, envir = cache)
  v
}

#' Score every cell of an aligned record set
#'
#' The scoring engine. Takes the paired-and-orphaned record layout produced by
#' [align_records()] and returns one row per record-by-field cell, carrying the
#' state that drives both the grid colours and the metric accounting.
#'
#' States: `agree`, `disagree`, `ai_missing` (paired, AI blank, gold has a
#' value), `gold_missing` (paired, gold blank, AI has a value), `ai_only`
#' (unpaired AI record), `gold_only` (unpaired gold record), `blank`.
#'
#' @param pairs A tibble from [align_records()] with `pair_id`, `paper`,
#'   `ai_rid`, `gold_rid`.
#' @param ai,gold Record tibbles keyed by `.rid`, with canonical field columns.
#' @param config A comparator configuration tibble; see
#'   [default_comparator_config()].
#' @param judge Optional judge function; see [compare_pair()].
#' @param cache Optional judge verdict cache environment.
#' @param overrides Optional tibble of manual cell overrides with `pair_id`,
#'   `field`, `agree`.
#' @param progress Optional function called as `progress(i, n)` between pairs.
#' @param originals Optional `list(ai = , gold = )` of record tables holding
#'   each side's values before normalisation, keyed by `.rid` like `ai` and
#'   `gold`. What a reader is shown; `ai` and `gold` are what is compared.
#'
#' @return A tibble with one row per cell: `pair_id`, `paper`, `ai_rid`,
#'   `gold_rid`, `field`, `ai_value`, `gold_value` (the values compared),
#'   `ai_original`, `gold_original` (the values as read -- the same as the
#'   compared ones unless a normaliser changed them), `state`, `rung`, `score`,
#'   `rationale`, `pending`, `overridden`.
#' @export
score_cells <- function(pairs, ai, gold, config,
                        judge = NULL, cache = NULL, overrides = NULL,
                        progress = NULL, originals = NULL) {
  config <- config[config$include, , drop = FALSE]
  fields <- config$field
  if (!nrow(pairs) || !length(fields)) {
    return(empty_cells())
  }

  ai_idx <- match(pairs$ai_rid, ai$.rid)
  gold_idx <- match(pairs$gold_rid, gold$.rid)
  ai_orig <- originals$ai %||% ai
  gold_orig <- originals$gold %||% gold
  ai_oidx <- match(pairs$ai_rid, ai_orig$.rid)
  gold_oidx <- match(pairs$gold_rid, gold_orig$.rid)
  n <- nrow(pairs)
  out <- vector("list", n)

  for (i in seq_len(n)) {
    if (!is.null(progress)) progress(i, n)
    rows <- lapply(seq_along(fields), function(k) {
      field <- fields[[k]]
      av <- cell_value(ai, ai_idx[[i]], field)
      gv <- cell_value(gold, gold_idx[[i]], field)
      has_ai <- !is.na(ai_idx[[i]])
      has_gold <- !is.na(gold_idx[[i]])
      blank_a <- isTRUE(is_blank(list(av))[[1L]])
      blank_b <- isTRUE(is_blank(list(gv))[[1L]])

      if (!has_gold) {
        state <- if (blank_a) "blank" else "ai_only"
        res <- cmp_result(FALSE, "unpaired", NA_real_)
      } else if (!has_ai) {
        state <- if (blank_b) "blank" else "gold_only"
        res <- cmp_result(FALSE, "unpaired", NA_real_)
      } else {
        res <- compare_pair(
          av, gv,
          comparator = config$comparator[[k]],
          threshold = config$threshold[[k]],
          tolerance = config$tolerance[[k]],
          set_mode = config$set_mode[[k]],
          judge = judge, field = field, cache = cache
        )
        state <- if (blank_a && blank_b) "blank"
        else if (blank_a) "ai_missing"
        else if (blank_b) "gold_missing"
        else if (res$agree) "agree" else "disagree"
      }

      tibble::tibble(
        pair_id = pairs$pair_id[[i]],
        paper = pairs$paper[[i]],
        ai_rid = pairs$ai_rid[[i]],
        gold_rid = pairs$gold_rid[[i]],
        field = field,
        ai_value = as_scalar_chr(av),
        gold_value = as_scalar_chr(gv),
        ai_original = as_scalar_chr(cell_value(ai_orig, ai_oidx[[i]], field)),
        gold_original = as_scalar_chr(cell_value(gold_orig, gold_oidx[[i]], field)),
        state = state,
        rung = res$rung,
        score = res$score,
        rationale = res$rationale,
        pending = isTRUE(res$pending),
        overridden = FALSE
      )
    })
    out[[i]] <- dplyr::bind_rows(rows)
  }

  cells <- dplyr::bind_rows(out)
  apply_overrides(cells, overrides)
}

#' Apply manual cell overrides
#'
#' An LLM verdict is a proposal; the human has the last word, and every
#' override is recorded rather than silently applied.
#'
#' @param cells A cell tibble from [score_cells()].
#' @param overrides A tibble with `pair_id`, `field`, `agree`.
#' @return `cells` with the overridden rows restated.
#' @export
apply_overrides <- function(cells, overrides = NULL) {
  if (is.null(overrides) || !nrow(overrides)) return(cells)
  key <- paste(cells$pair_id, cells$field)
  okey <- paste(overrides$pair_id, overrides$field)
  hit <- match(key, okey)
  idx <- which(!is.na(hit) & cells$state %in%
                 c("agree", "disagree", "ai_missing", "gold_missing"))
  if (!length(idx)) return(cells)
  agree <- as.logical(overrides$agree[hit[idx]])
  cells$state[idx] <- ifelse(agree, "agree", "disagree")
  cells$rung[idx] <- "override"
  cells$pending[idx] <- FALSE
  cells$overridden[idx] <- TRUE
  cells
}

#' @keywords internal
#' @noRd
cell_value <- function(df, i, field) {
  if (is.na(i) || !field %in% names(df)) return(NA_character_)
  col <- df[[field]]
  if (is.list(col)) col[[i]] else col[[i]]
}

#' An empty cell table
#'
#' The zero-row prototype [score_cells()] returns when there is nothing to
#' score. Useful for feeding the metric and plot functions before a run has
#' produced anything.
#'
#' @return A zero-row tibble with the cell table's columns.
#' @export
empty_cells <- function() {
  empty_tbl(
    pair_id = character(), paper = character(), ai_rid = character(),
    gold_rid = character(), field = character(), ai_value = character(),
    gold_value = character(), ai_original = character(),
    gold_original = character(), state = character(), rung = character(),
    score = numeric(), rationale = character(), pending = logical(),
    overridden = logical()
  )
}

#' Similarity scores across a column's actual candidate pairs
#'
#' Feeds the threshold-picking chart: the user places a fuzzy cutoff by looking
#' at the distribution over their own data rather than guessing.
#'
#' @param pairs,ai,gold As in [score_cells()].
#' @param field The field to profile.
#' @return A tibble with `pair_id`, `ai_value`, `gold_value`, `score`.
#' @export
similarity_profile <- function(pairs, ai, gold, field) {
  pairs <- pairs[!is.na(pairs$ai_rid) & !is.na(pairs$gold_rid), , drop = FALSE]
  if (!nrow(pairs) || !field %in% names(ai) || !field %in% names(gold)) {
    return(empty_tbl(pair_id = character(), ai_value = character(),
                     gold_value = character(), score = numeric()))
  }
  ai_idx <- match(pairs$ai_rid, ai$.rid)
  gold_idx <- match(pairs$gold_rid, gold$.rid)
  av <- vapply(ai_idx, function(i) as_scalar_chr(cell_value(ai, i, field)), character(1))
  gv <- vapply(gold_idx, function(i) as_scalar_chr(cell_value(gold, i, field)), character(1))
  tibble::tibble(
    pair_id = pairs$pair_id,
    ai_value = av,
    gold_value = gv,
    score = similarity(av, gv)
  )
}
