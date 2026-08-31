fixture_findings <- function(fx) {
  conf <- dplyr::bind_rows(
    check_conformance(fx$ai, fx$schema, "ai"),
    check_conformance(fx$gold, fx$schema, "gold")
  )
  gold_raw <- read_table_any(fixture("gold_records.csv"))
  collect_findings(
    fx$cells, fx$pairs, conf, fx$schema,
    gold_fields = names(gold_raw),
    collapses = granularity_check(fx$gold, fx$linkage, "gold"),
    dropped_fields = "habitat_notes",
    linkage_fields = fx$linkage
  )
}

test_that("findings are grouped by who acts on them", {
  fx <- fixture_run()
  f <- fixture_findings(fx)
  expect_true(all(f$group %in% c("schema", "model", "gold")))
  expect_setequal(unique(f$group), c("schema", "model", "gold"))
})

test_that("a recurring out-of-enum value is a schema finding", {
  fx <- fixture_run()
  f <- fixture_findings(fx)
  schema_rows <- f[f$group == "schema", ]
  expect_match(paste(schema_rows$detail, collapse = " "), "commensalism")
})

test_that("a one-off out-of-enum value is a gold-standard finding, not a schema one", {
  fx <- fixture_run()
  f <- fixture_findings(fx)
  expect_match(paste(f$detail[f$group == "gold"], collapse = " "), "predatoin")
  expect_false(grepl("predatoin", paste(f$detail[f$group == "schema"], collapse = " ")))
})

test_that("a gold column the schema has no slot for is reported", {
  fx <- fixture_run()
  f <- fixture_findings(fx)
  expect_match(paste(f$detail, collapse = " "), "habitat_notes")
})

test_that("the granularity mismatch reaches the findings", {
  fx <- fixture_run()
  f <- fixture_findings(fx)
  expect_true(any(grepl("collapse", f$detail)))
})

test_that("source consistency catches records outside the paper list", {
  recs <- tibble::tibble(.rid = c("r1", "r2"), .paper = c("P1", "P2"))
  papers <- tibble::tibble(.paper = "P1")
  out <- source_consistency(recs, papers, "gold")
  expect_true("records_outside_paper_list" %in% out$check)
  # Say the consequence out loud rather than quietly patching it.
  expect_match(out$detail[out$check == "records_outside_paper_list"],
               "cannot be trusted")
})

test_that("source consistency catches unresolvable and duplicate papers", {
  recs <- tibble::tibble(.rid = c("r1", "r2"), .paper = c(NA, "P1"))
  papers <- tibble::tibble(.paper = c("P1", "P1"))
  out <- source_consistency(recs, papers, "ai")
  expect_true("unresolvable_paper" %in% out$check)
  expect_true("duplicate_paper_id" %in% out$check)
})

test_that("only two conditions actually block", {
  expect_null(blocking_condition(list(papers = "P"), "field"))
  expect_match(blocking_condition(list(papers = character(0)), "field"),
               "nothing to compare")
  expect_match(blocking_condition(list(papers = "P"), character(0)),
               "nothing to score")
})

test_that("the schema patch is applicable, not prose", {
  fx <- fixture_run()
  conf <- dplyr::bind_rows(
    check_conformance(fx$ai, fx$schema, "ai"),
    check_conformance(fx$gold, fx$schema, "gold")
  )
  patch <- schema_patch(conf, granularity_check(fx$gold, fx$linkage, "gold"),
                        fx$cells, fx$schema)
  expect_true("commensalism" %in% patch$enum_additions$interaction_type)
  # A typo seen once does not earn a place in the enum.
  expect_false("predatoin" %in% patch$enum_additions$interaction_type)
  expect_true(!is.null(patch[["x-unique-fields"]]))
})
