test_that("a run configuration round-trips through disk unchanged", {
  fx <- fixture_run()
  cfg <- capture_run_config(
    new_run_config(),
    comparators = fx$config,
    paper_map = fx$paper_map,
    scope = fx$scope,
    metrics = list(f1 = 0.71, overall_accuracy = 0.66)
  )
  path <- withr::local_tempfile(fileext = ".json")
  write_run_config(cfg, path)
  back <- read_run_config(path)

  tables <- restore_run_tables(back)
  expect_equal(tables$comparators$field, fx$config$field)
  expect_equal(tables$comparators$comparator, fx$config$comparator)
  expect_equal(tables$comparators$linkage, fx$config$linkage)
  expect_setequal(back$linkage_fields, fx$linkage)
  expect_equal(back$scope$papers, fx$scope$papers)
})

test_that("manual decisions survive a reload, because they are real labour", {
  cfg <- capture_run_config(
    new_run_config(),
    rejected = tibble::tibble(ai_rid = "a1", gold_rid = "g1"),
    added = tibble::tibble(ai_rid = "a2", gold_rid = "g2"),
    overrides = tibble::tibble(pair_id = "p1", field = "f", agree = TRUE)
  )
  path <- withr::local_tempfile(fileext = ".json")
  write_run_config(cfg, path)
  t <- restore_run_tables(read_run_config(path))
  expect_equal(t$rejected$ai_rid, "a1")
  expect_equal(t$added$gold_rid, "g2")
  expect_true(t$overrides$agree)
})

test_that("cached judge verdicts are frozen into the configuration", {
  cache <- new_cache()
  judge <- function(field, a, b) list(agree = TRUE, rationale = "same claim")
  compare_pair("x y", "y z q", "judge", judge = judge, cache = cache)

  cfg <- capture_run_config(new_run_config(), cache = cache)
  path <- withr::local_tempfile(fileext = ".json")
  write_run_config(cfg, path)
  back <- read_run_config(path)
  expect_length(back$judge_cache, 1L)

  # A reloaded cache answers without calling the judge again.
  restored <- new_cache(back$judge_cache)
  calls <- 0L
  counting <- function(field, a, b) { calls <<- calls + 1L; list(agree = FALSE) }
  res <- compare_pair("x y", "y z q", "judge", judge = counting, cache = restored)
  expect_equal(calls, 0L)
  expect_true(res$agree)
})

test_that("an empty table round-trips as an empty table, not as NULL", {
  cfg <- capture_run_config(new_run_config(),
                            rejected = run_config_protos()$links)
  path <- withr::local_tempfile(fileext = ".json")
  write_run_config(cfg, path)
  t <- restore_run_tables(read_run_config(path))
  expect_equal(nrow(t$rejected), 0L)
  expect_true(all(c("ai_rid", "gold_rid") %in% names(t$rejected)))
})

test_that("a configuration from a future format version warns rather than fails", {
  cfg <- new_run_config()
  cfg$format_version <- 99L
  path <- withr::local_tempfile(fileext = ".json")
  write_run_config(cfg, path)
  expect_warning(read_run_config(path), "format version")
})

test_that("hand-tuned results say so", {
  expect_null(correction_count())
  expect_equal(
    correction_count(tibble::tibble(a = 1:2), tibble::tibble(a = 1),
                     tibble::tibble(a = 1:3)),
    "2 links rejected, 1 added, 3 cells overridden"
  )
})

test_that("runs diff on the numbers that moved", {
  before <- capture_run_config(new_run_config(), metrics = list(f1 = 0.71, acc = 0.6))
  after <- capture_run_config(new_run_config(), metrics = list(f1 = 0.78, acc = 0.6))
  d <- diff_runs(before, after)
  expect_equal(d$metric[[1L]], "f1")
  expect_equal(round(d$delta[[1L]], 3), 0.07)
})
