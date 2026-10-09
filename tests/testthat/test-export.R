test_that("the bundle holds everything the design says it does", {
  fx <- fixture_run()
  dir <- withr::local_tempdir()
  out <- export_bundle(
    file.path(dir, "run"), fx$cells, fx$pairs,
    capture_run_config(new_run_config(), comparators = fx$config, scope = fx$scope),
    scope = fx$scope, schema = fx$schema, plots = FALSE
  )
  expect_true(all(file.exists(file.path(out, c(
    "aligned_table.csv", "aligned_table_long.csv", "metrics.xlsx",
    "findings.csv", "run_config.json", "README.txt"
  )))))
})

test_that("the aligned table mirrors the grid, one row per aligned record", {
  fx <- fixture_run()
  wide <- aligned_table(fx$cells, fx$pairs)
  expect_equal(nrow(wide), nrow(fx$pairs))
  expect_true(all(c("ai_interaction_type", "gold_interaction_type",
                    "state_interaction_type") %in% names(wide)))
})

test_that("the long table carries the colour the grid painted", {
  fx <- fixture_run()
  long <- aligned_table_long(fx$cells)
  expect_true("colour" %in% names(long))
  # Colour follows the outcome, not the alignment detail: a value only the AI
  # has is orange whether it came from an unpaired record or a blank gold cell.
  expect_setequal(unique(long$colour[long$state == "ai_only"]), "orange")
  expect_setequal(unique(long$colour[long$state == "gold_only"]), "yellow")
  expect_setequal(unique(long$colour[long$state == "disagree"]), "purple")
  expect_equal(long$colour, unname(state_colour(long$state)))
})

test_that("the workbook has a summary sheet and one sheet per field", {
  fx <- fixture_run()
  path <- withr::local_tempfile(fileext = ".xlsx")
  write_metrics_workbook(path, fx$cells, fx$pairs, fx$schema)
  sheets <- openxlsx::getSheetNames(path)
  expect_true("summary" %in% sheets)
  expect_true(length(sheets) > 1L)
})

test_that("caveats travel with the export instead of living only on screen", {
  fx <- fixture_run()
  readme <- bundle_readme(
    fx$scope,
    warnings = "The AI has no paper list.",
    progress = progress_summary(fx$cells, fx$scope$papers),
    corrections = "1 links rejected, 0 added, 2 cells overridden",
    agg = aggregate_metrics(fx$cells),
    rm_ = record_metrics(fx$pairs)
  )
  txt <- paste(readme, collapse = "\n")
  expect_match(txt, "SCOPE")
  expect_match(txt, "ACTIVE WARNINGS")
  expect_match(txt, "no paper list")
  expect_match(txt, "papers judged")
  expect_match(txt, "Manual corrections")
})

test_that("plots render without erroring on real scored data", {
  fx <- fixture_run()
  expect_s3_class(plot_column_accuracy(field_metrics(fx$cells)), "ggplot")
  expect_s3_class(plot_completeness(fill_rates(fx$cells)), "ggplot")
  expect_s3_class(plot_record_outcome(record_metrics(fx$pairs)), "ggplot")
  expect_s3_class(plot_confusion(column_confusion(fx$cells, "interaction_type",
                                                  fx$schema)), "ggplot")
  expect_s3_class(plot_threshold(
    similarity_profile(fx$pairs, fx$ai, fx$gold, "bat_species_scientific_name")),
    "ggplot")
})

test_that("plots degrade to an explanation rather than erroring when empty", {
  expect_s3_class(plot_column_accuracy(field_metrics(empty_cells())), "ggplot")
  expect_s3_class(plot_completeness(fill_rates(empty_cells())), "ggplot")
})
