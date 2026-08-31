test_that("read_schema finds the record object inside an array wrapper", {
  s <- read_schema(fixture("schema.json"))
  expect_s3_class(s, "ecoeval_schema")
  expect_true("bat_species_scientific_name" %in% s$fields$field)
  expect_equal(s$unique_fields,
               c("bat_species_scientific_name", "interaction_type"))
})

test_that("read_schema accepts a bare record object", {
  s <- read_schema(list(type = "object", properties = list(
    a = list(type = "string"), b = list(type = "integer")
  )))
  expect_equal(sort(s$fields$field), c("a", "b"))
})

test_that("read_schema rejects something that is not a schema", {
  expect_error(read_schema(list(a = 1, b = 2)), class = "ecoeval_bad_schema")
})

test_that("union types collapse to the non-null member", {
  s <- read_schema(list(type = "object", properties = list(
    a = list(type = list("string", "null"))
  )))
  expect_equal(s$fields$type, "string")
})

test_that("comparator defaults follow the type", {
  s <- read_schema(fixture("schema.json"))
  cmp <- stats::setNames(s$fields$comparator, s$fields$field)
  expect_equal(unname(cmp[["interaction_type"]]), "exact")       # enum
  expect_equal(unname(cmp[["year_observed"]]), "numeric")        # integer
  expect_equal(unname(cmp[["observation_date"]]), "date")        # date format
  expect_equal(unname(cmp[["location_country"]]), "normalized")  # plain string
  # Free prose goes to the judge, whose cascade still runs the cheap rungs
  # first -- nothing costs money until they have failed.
  expect_equal(unname(cmp[["all_supporting_source_sentences"]]), "judge")
})

test_that("x-unique-fields seeds the linkage suggestion but is only a default", {
  s <- read_schema(fixture("schema.json"))
  cfg <- default_comparator_config(s)
  expect_setequal(cfg$field[cfg$linkage], s$unique_fields)

  override <- default_comparator_config(s, linkage = "location_country")
  expect_equal(override$field[override$linkage], "location_country")
})

test_that("conformance reports out-of-enum values with their frequencies", {
  fx <- fixture_run()
  conf <- check_conformance(fx$gold, fx$schema, "gold")
  it <- conf[conf$field == "interaction_type", ]
  expect_true("commensalism" %in% it$value)
  expect_equal(it$n[it$value == "commensalism"], 3L)
  # A one-off is a typo, not a missing category. The counts are what make the
  # difference obvious without a classifier.
  expect_equal(it$n[it$value == "predatoin"], 1L)
  expect_true(all(it$reason == "not_in_enum"))
})

test_that("conformance is clean on the AI side of the fixtures", {
  fx <- fixture_run()
  conf <- check_conformance(fx$ai, fx$schema, "ai")
  expect_equal(nrow(conf[conf$field == "interaction_type", ]), 0L)
})

test_that("unschematised_fields names gold columns the schema has no slot for", {
  fx <- fixture_run()
  gold_raw <- read_table_any(fixture("gold_records.csv"))
  expect_true("habitat_notes" %in%
                unschematised_fields(names(gold_raw), fx$schema))
})
