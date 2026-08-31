test_that("a wrongly linked pair scores the same as leaving it unlinked", {
  # The property the whole accounting rests on: linking earns nothing except
  # where fields genuinely agree, so bad links cannot inflate field accuracy.
  ai <- tibble::tibble(.rid = "a1", .paper = "P", v = "alpha", w = "one")
  gold <- tibble::tibble(.rid = "g1", .paper = "P", v = "omega", w = "two")
  cfg <- default_comparator_config(fields = c("v", "w"))

  linked <- score_cells(
    tibble::tibble(pair_id = "p1", paper = "P", ai_rid = "a1", gold_rid = "g1"),
    ai, gold, cfg)
  unlinked <- score_cells(
    tibble::tibble(pair_id = c("p1", "p2"), paper = "P",
                   ai_rid = c("a1", NA), gold_rid = c(NA, "g1")),
    ai, gold, cfg)

  cols <- c("field", "tp", "fp", "fn")
  expect_equal(field_metrics(linked)[, cols], field_metrics(unlinked)[, cols])
})

test_that("each state contributes what the design says it does", {
  states <- c("agree", "disagree", "ai_missing", "gold_missing",
              "ai_only", "gold_only", "blank")
  cells <- tibble::tibble(
    pair_id = paste0("p", seq_along(states)), paper = "P",
    ai_rid = "a", gold_rid = "g", field = "f",
    ai_value = "x", gold_value = "y", state = states,
    rung = "exact", score = 1, rationale = NA_character_,
    pending = FALSE, overridden = FALSE
  )
  fm <- field_metrics(cells)
  expect_equal(fm$tp, 1L)                      # agree
  expect_equal(fm$fp, 3L)                      # disagree, gold_missing, ai_only
  expect_equal(fm$fn, 3L)                      # disagree, ai_missing, gold_only
  expect_equal(fm$tn, 1L)                      # blank
  expect_equal(fm$n_scored, 6L)                # everything but blank
})

test_that("both-blank cells drop out of precision and recall", {
  cells <- tibble::tibble(
    pair_id = "p1", paper = "P", ai_rid = "a", gold_rid = "g", field = "f",
    ai_value = NA_character_, gold_value = NA_character_, state = "blank",
    rung = "blank", score = 1, rationale = NA_character_,
    pending = FALSE, overridden = FALSE
  )
  fm <- field_metrics(cells)
  expect_equal(fm$n_scored, 0L)
  expect_true(is.na(fm$accuracy))
  expect_true(is.na(fm$precision))
})

test_that("record metrics count leftovers as false positives and negatives", {
  fx <- fixture_run()
  rm_ <- record_metrics(fx$pairs)
  expect_equal(rm_$tp, sum(fx$pairs$kind == "pair"))
  expect_equal(rm_$fp, sum(fx$pairs$kind == "ai_only"))
  expect_equal(rm_$fn, sum(fx$pairs$kind == "gold_only"))
  expect_equal(rm_$precision, rm_$tp / (rm_$tp + rm_$fp))
})

test_that("column accuracy is sorted worst first, for triage", {
  fx <- fixture_run()
  acc <- stats::na.omit(field_metrics(fx$cells)$accuracy)
  expect_false(is.unsorted(acc))
})

test_that("the two aggregates diverge only when fill rates are uneven", {
  fx <- fixture_run()
  agg <- aggregate_metrics(fx$cells)
  expect_true(agg$overall_accuracy >= 0 && agg$overall_accuracy <= 1)
  expect_equal(agg$n_columns, dplyr::n_distinct(fx$cells$field))
})

test_that("an enum column gets a true K-by-K with an out-of-schema class", {
  fx <- fixture_run()
  cc <- column_confusion(fx$cells, "interaction_type", fx$schema)
  expect_equal(cc$type, "class")
  expect_true("(not in schema)" %in% cc$matrix$gold_class)
  # The gold standard's commensalism records land there rather than vanishing.
  expect_true(sum(cc$matrix$n[cc$matrix$gold_class == "(not in schema)"]) > 0)
})

test_that("an absent record is a class on its own axis only", {
  fx <- fixture_run()
  cc <- column_confusion(fx$cells, "interaction_type", fx$schema)
  expect_false("(no AI record)" %in% cc$matrix$gold_class)
  expect_false("(no gold record)" %in% cc$matrix$ai_class)
})

test_that("free text gets presence-absence with the correct/wrong split", {
  fx <- fixture_run()
  cc <- column_confusion(fx$cells, "all_supporting_source_sentences", fx$schema)
  expect_equal(cc$type, "presence")
  expect_true(all(c("correct", "wrong value") %in% cc$matrix$ai_class))
})

test_that("a numeric column reports how wrong the numbers are", {
  fx <- fixture_run()
  cc <- column_confusion(fx$cells, "sample_size", fx$schema)
  expect_equal(cc$type, "numeric")
  expect_true(is.numeric(cc$errors$error))
})

test_that("a column too thin to read carries a caution", {
  cells <- tibble::tibble(
    pair_id = "p1", paper = "P", ai_rid = "a", gold_rid = "g", field = "f",
    ai_value = "x", gold_value = "x", state = "agree", rung = "exact",
    score = 1, rationale = NA_character_, pending = FALSE, overridden = FALSE
  )
  expect_match(column_confusion(cells, "f")$notes, "too few", all = FALSE)
})

test_that("fill rates surface the asymmetry that reads as a schema problem", {
  fx <- fixture_run()
  fills <- fill_rates(fx$cells)
  dm <- fills[fills$field == "detection_method", ]
  # Humans always record how they detected it; the AI usually does not.
  expect_true(dm$gold_fill > dm$ai_fill)
})

test_that("triage flags the pair whose identity columns all disagree", {
  fx <- fixture_run()
  susp <- suspect_pairs(fx$cells, fx$linkage)
  expect_true(nrow(susp) >= 1L)
  expect_true(all(susp$reason %in%
                    c("agrees on nothing", "identity columns all disagree")))
})

test_that("progress distinguishes unjudged from judged from reviewed", {
  fx <- fixture_run()
  prog <- progress_summary(fx$cells, fx$scope$papers, reviewed = fx$scope$papers[1])
  expect_equal(prog$n_papers, length(fx$scope$papers))
  expect_equal(prog$n_reviewed, 1L)
  # Free prose is left for the judge, so some papers are not yet judged.
  expect_true(prog$n_judged < prog$n_papers)
  expect_true(prog$n_pending_cells > 0L)
})
