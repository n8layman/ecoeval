test_that("the cascade stops at the cheapest rung that resolves a pair", {
  expect_equal(compare_pair("a", "a", "fuzzy")$rung, "exact")
  expect_equal(compare_pair("A ", "a", "fuzzy")$rung, "normalized")
  expect_equal(compare_pair("Myotis lucifugus", "Myotis lucifigus", "fuzzy")$rung,
               "fuzzy")
})

test_that("a comparator never climbs past its configured rung", {
  # Exact must not quietly accept a case difference.
  expect_false(compare_pair("Predation", "predation", "exact")$agree)
  expect_true(compare_pair("Predation", "predation", "normalized")$agree)
  # Normalized must not quietly accept a near miss.
  expect_false(compare_pair("Myotis lucifugus", "Myotis lucifigus", "normalized")$agree)
})

test_that("blank cells are recognised before any rung runs", {
  expect_equal(compare_pair(NA, NA, "fuzzy")$rung, "blank")
  expect_true(compare_pair(NA, NA, "fuzzy")$agree)
  expect_false(compare_pair("a", NA, "fuzzy")$agree)
})

test_that("numeric comparison sees through formatting", {
  expect_true(compare_pair("10", "10.0", "numeric")$agree)
  expect_true(compare_pair(10, 10.0000001, "numeric", tolerance = 1e-3)$agree)
  expect_false(compare_pair("10", "11", "numeric")$agree)
  expect_false(compare_pair("ten", "10", "numeric")$agree)
})

test_that("dates compare by value, not by format", {
  expect_true(compare_pair("2021-03-08", "08/03/2021", "date")$agree)
  expect_true(compare_pair("2016-11-19", "19 November 2016", "date")$agree)
  expect_false(compare_pair("2021-03-08", "2021-03-09", "date")$agree)
})

test_that("parse_date_loose does not let strptime swallow trailing text", {
  # "05/01/2020" under "%Y/%m/%d" would otherwise parse as the year 5.
  expect_equal(parse_date_loose("05/01/2020"), as.Date("2020-01-05"))
})

test_that("partial dates pin to the start of the period, not today", {
  expect_equal(parse_date_loose("2020"), as.Date("2020-01-01"))
  expect_equal(parse_date_loose("2020-03"), as.Date("2020-03-01"))
})

test_that("unparseable text is NA rather than a wrong date", {
  expect_true(is.na(parse_date_loose("sometime in the spring")))
})

test_that("a whole column is read under one house style", {
  expect_equal(parse_date_loose(c("03/04/2020", "15/04/2020")),
               as.Date(c("2020-04-03", "2020-04-15")))
})

test_that("set comparison is order- and delimiter-insensitive", {
  expect_true(compare_pair("a; b", "b|a", "set", set_mode = "exact")$agree)
  expect_false(compare_pair("a; b", "a", "set", set_mode = "exact")$agree)
  expect_true(compare_pair("a; b", "a", "set", set_mode = "overlap")$agree)
  expect_equal(set_score("a; b", "a; c", "jaccard"), 1 / 3)
})

test_that("similarity is bounded and blank-safe", {
  expect_equal(similarity("abc", "abc"), 1)
  expect_true(similarity("abc", "abd") < 1)
  expect_true(all(similarity(c("a", NA), c("a", "b"))[2] %in% c(NA_real_, 0)))
})

test_that("the judge rung marks cells pending when no judge is configured", {
  res <- compare_pair("one wording", "an entirely different wording", "judge")
  expect_true(res$pending)
  expect_equal(res$rung, "fuzzy")
  expect_false(res$agree)
})

test_that("a configured judge is consulted only after the cheap rungs fail", {
  calls <- 0L
  judge <- function(field, a, b) {
    calls <<- calls + 1L
    list(agree = TRUE, rationale = "same claim, different words")
  }
  same <- compare_pair("a", "a", "judge", judge = judge)
  expect_equal(calls, 0L)
  expect_equal(same$rung, "exact")

  diff <- compare_pair("bats ate moths", "moths were eaten by bats", "judge",
                       judge = judge)
  expect_equal(calls, 1L)
  expect_equal(diff$rung, "judge")
  expect_true(diff$agree)
  expect_match(diff$rationale, "different words")
  expect_false(diff$pending)
})

test_that("judge verdicts are cached per pair, so a run is reproducible", {
  calls <- 0L
  judge <- function(field, a, b) {
    calls <<- calls + 1L
    list(agree = TRUE, rationale = "cached")
  }
  cache <- new_cache()
  for (i in 1:3) compare_pair("x y", "y z q", "judge", judge = judge, cache = cache)
  expect_equal(calls, 1L)
  expect_length(cache_as_list(cache), 1L)
})

test_that("a judge that errors degrades to a disagreement, not a crash", {
  judge <- function(field, a, b) stop("no network")
  res <- compare_pair("x y", "y z q", "judge", judge = judge)
  expect_false(res$agree)
  expect_match(res$rationale, "judge failed")
})

test_that("score_cells labels every state the grid and the metrics rely on", {
  ai <- tibble::tibble(.rid = c("a1", "a2"), .paper = "P", v = c("x", "only ai"))
  gold <- tibble::tibble(.rid = c("g1", "g2"), .paper = "P", v = c("x", "only gold"))
  pairs <- tibble::tibble(
    pair_id = c("p1", "p2", "p3"), paper = "P",
    ai_rid = c("a1", "a2", NA), gold_rid = c("g1", NA, "g2")
  )
  cfg <- default_comparator_config(fields = "v")
  cells <- score_cells(pairs, ai, gold, cfg)
  expect_equal(cells$state, c("agree", "ai_only", "gold_only"))
})

test_that("a blank on one side of a pair is missing, not a disagreement", {
  ai <- tibble::tibble(.rid = "a1", .paper = "P", v = NA_character_)
  gold <- tibble::tibble(.rid = "g1", .paper = "P", v = "something")
  pairs <- tibble::tibble(pair_id = "p1", paper = "P", ai_rid = "a1", gold_rid = "g1")
  cells <- score_cells(pairs, ai, gold, default_comparator_config(fields = "v"))
  expect_equal(cells$state, "ai_missing")
})

test_that("an override has the last word over any comparator verdict", {
  fx <- fixture_run()
  cells <- fx$cells
  target <- cells[cells$state == "disagree", ][1, ]
  ov <- tibble::tibble(pair_id = target$pair_id, field = target$field, agree = TRUE)
  out <- apply_overrides(cells, ov)
  row <- out[out$pair_id == target$pair_id & out$field == target$field, ]
  expect_equal(row$state, "agree")
  expect_equal(row$rung, "override")
  expect_true(row$overridden)
})

test_that("an override cannot invent a pairing that does not exist", {
  fx <- fixture_run()
  orphan <- fx$cells[fx$cells$state == "ai_only", ][1, ]
  ov <- tibble::tibble(pair_id = orphan$pair_id, field = orphan$field, agree = TRUE)
  out <- apply_overrides(fx$cells, ov)
  row <- out[out$pair_id == orphan$pair_id & out$field == orphan$field, ]
  expect_equal(row$state, "ai_only")
})
