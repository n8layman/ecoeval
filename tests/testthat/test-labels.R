# What the two sides are called (#20). Wording only: the metrics never change.

test_that("side labels default to AI and gold standard, and validate", {
  l <- side_labels()
  expect_equal(c(l$ai, l$gold), c("AI", "Gold standard"))
  expect_false(l$neutral)
  expect_error(side_labels(ai = ""), "non-empty")
  expect_error(as_side_labels(c(left = "x")), "side_labels")
  l <- as_side_labels(c(ai = "Extraction", gold = "Reference"))
  expect_s3_class(l, "ecoeval_labels")
  expect_equal(l$gold, "Reference")
})

test_that("outcome labels follow the names, and neutral drops the FP/FN wording", {
  ol <- outcome_labels(side_labels("Extraction", "Reference"))
  expect_equal(ol[["only_ai"]], "Extraction only (FP)")
  expect_equal(ol[["only_gold"]], "Reference only (FN)")
  neutral <- outcome_labels(side_labels("Extraction", "Reference", neutral = TRUE))
  expect_equal(neutral[["only_ai"]], "Extraction only")
  expect_false(any(grepl("TP|FP|FN", neutral)))
  expect_equal(names(neutral), names(ol))
})

test_that("use_side_labels sets the session default and gives back the old one", {
  old <- use_side_labels(c(ai = "Parser", gold = "Filed form"))
  on.exit(options(ecoeval.labels = old))
  expect_equal(current_labels()$ai, "Parser")
  expect_equal(outcome_labels()[["only_gold"]], "Filed form only (FN)")
  use_side_labels(NULL)
  expect_equal(current_labels()$ai, "AI")
})

test_that("plots, tables, findings and README use the labels; metrics do not change", {
  fx <- fixture_run()
  l <- side_labels("Extraction", "Reference", neutral = TRUE)

  ct <- confusion_totals(fx$cells, labels = l)
  base <- confusion_totals(fx$cells)
  expect_equal(ct[c("tp", "fp", "fn", "tn", "precision", "recall")],
               base[c("tp", "fp", "fn", "tn", "precision", "recall")])
  expect_true("Only Reference has a value" %in% ct$by_outcome$label)
  expect_false(any(grepl("false", ct$by_outcome$contributes)))

  cc <- column_confusion(fx$cells, "interaction_type", fx$schema, labels = l)
  expect_true(any(grepl("(no Reference record)", cc$matrix$gold_class, fixed = TRUE)))
  p <- plot_confusion(cc, l)
  expect_equal(p$labels$x, "Extraction")
  expect_equal(p$labels$y, "Reference")

  grid <- record_field_outcomes(fx$cells, fx$scope$papers[[1L]])
  expect_true(all(grid$kind %in% c("matched", "AI only", "gold only")))
  ph <- plot_record_heatmap(grid, labels = l)
  expect_true(any(grepl("^Extraction: ", unlist(strsplit(ph$data$text, "\n")))))
  expect_false(any(grepl("\\(FP\\)|\\(FN\\)", ph$data$text)))
  expect_equal(record_kind_label(c("matched", "AI only", "gold only"), l),
               c("matched", "Extraction only", "Reference only"))

  ro <- plot_record_outcome(record_metrics(fx$pairs), l)
  expect_setequal(levels(ro$data$outcome),
                  c("Matched", "Extraction only", "Reference only"))
  expect_true(all(c("Extraction", "Reference") %in%
                    as.character(plot_completeness(fill_rates(fx$cells), l)$data$source)))

  f <- collect_findings(fx$cells, fx$pairs, labels = l)
  expect_false(any(grepl("false positives", f$detail)))
  expect_true(any(grepl("Extraction only", f$detail)))

  readme <- bundle_readme(fx$scope, labels = l)
  expect_true(any(grepl("papers Extraction only", readme)))
})

test_that("a run configuration keeps the labels", {
  path <- withr::local_tempfile(fileext = ".json")
  cfg <- capture_run_config(new_run_config(),
                            labels = side_labels("Extraction", "Reference", TRUE))
  write_run_config(cfg, path)
  back <- read_run_config(path)
  expect_s3_class(back$side_labels, "ecoeval_labels")
  expect_equal(back$side_labels$gold, "Reference")
  expect_true(back$side_labels$neutral)
  expect_null(read_run_config({
    write_run_config(new_run_config(), path); path
  })$side_labels)
})
