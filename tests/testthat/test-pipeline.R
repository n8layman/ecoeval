# The pipeline stages run_eval_app() and evaluate_extraction() share, and the
# setup inputs they accept in place of the setup screens.

ex_args <- function(...) {
  utils::modifyList(list(
    ai = fixture("ai_records.csv"), gold = fixture("gold_records.csv"),
    schema = fixture("schema.json")
  ), list(...))
}

test_that("setup stops at the first stage it was not given an input for", {
  run <- function(...) do.call(setup_evaluation, ex_args(skip_setup = FALSE, ...))
  expect_equal(setup_evaluation(skip_setup = FALSE)$stage, "load")
  expect_equal(run()$stage, "metadata")
  expect_equal(run(paper_key = "doi", auto_accept = FALSE)$stage, "papers")
  expect_equal(run(paper_key = "doi")$stage, "fields")
  done <- run(paper_key = "doi", linkage_fields = "interaction_type")
  expect_equal(done$stage, "scored")
  expect_true(nrow(done$scored$cells) > 0L)
})

test_that("skip_setup takes every default and scores", {
  run <- do.call(setup_evaluation, ex_args(skip_setup = TRUE))
  expect_equal(run$stage, "scored")
  expect_equal(run$keys$ai$strategy, "doi")
  expect_setequal(run$config$field[run$config$linkage],
                  c("bat_species_scientific_name", "interaction_type"))
})

test_that("a stage that fails stops there and says why", {
  run <- do.call(setup_evaluation, ex_args(paper_key = "nope"))
  expect_equal(run$stage, "metadata")
  expect_match(run$message, "nope")

  run <- do.call(setup_evaluation, ex_args(mapping = list(ai = c(sample_size = "zzz"))))
  expect_equal(run$stage, "fields")
  expect_match(run$message, "zzz")

  run <- do.call(setup_evaluation, ex_args(linkage_fields = character(0)))
  expect_equal(run$stage, "fields")
  expect_match(run$message, "identity column")
})

test_that("auto-accepting paper links says how many it left out", {
  run <- do.call(setup_evaluation, ex_args(
    ai_papers = fixture("ai_papers.csv"), gold_papers = fixture("gold_papers.csv")))
  expect_equal(run$stage, "scored")
  expect_equal(length(run$scoped$scope$papers), 10L)
  expect_true(any(grepl("1 paper link", run$warnings)))
})

test_that("a supplied paper map is the scope", {
  pm <- data.frame(ai_paper = c("10.1000/p01", "10.1000/p02"),
                   gold_paper = c("10.1000/p01", "10.1000/p02"))
  run <- do.call(setup_evaluation, ex_args(paper_map = pm))
  expect_equal(run$scoped$scope$papers, c("10.1000/p01", "10.1000/p02"))
  expect_null(run$proposal)
  expect_setequal(unique(run$scored$cells$paper), pm$ai_paper)
  expect_error(set_scope(run$placed, data.frame(a = 1)), "ai_paper")
})

test_that("record sets can be data frames shaped before the evaluation", {
  ai <- utils::read.csv(fixture("ai_records.csv"))
  ai <- ai[ai$doi != "10.1000/p01", ]
  run <- do.call(setup_evaluation, ex_args(ai = ai))
  expect_equal(run$stage, "scored")
  expect_null(run$loaded$inputs$ai)
  expect_equal(run$loaded$inputs$gold, fixture("gold_records.csv"))
  expect_false("10.1000/p01" %in% run$scoped$scope$papers)
})

test_that("paper keys can be given per table", {
  tabs <- list(ai = data.frame(doi = "x", title = "a"),
               gold = data.frame(DOI = "x", Title = "a"))
  keys <- choose_paper_keys(tabs, list(gold = "Title"))
  expect_equal(keys$gold$columns, "Title")
  expect_equal(keys$ai$columns, "doi")
  expect_error(choose_paper_keys(tabs, list(other = "x")), "named by table")
})

test_that("mapping, comparators, identity columns and fields override defaults", {
  schema <- read_schema(fixture("schema.json"))
  ai <- read_table_any(fixture("ai_records.csv"))
  gold <- read_table_any(fixture("gold_records.csv"))
  keys <- list(ai = paper_key("doi"), gold = paper_key("doi"))

  cfg <- configure_fields(
    schema, ai, gold, keys,
    mapping = list(gold = c(sample_size = NA, habitat = "habitat_notes")),
    comparator_config = data.frame(field = "location_country",
                                   comparator = "fuzzy", threshold = 0.7),
    linkage_fields = "interaction_type"
  )
  expect_true(is.na(cfg$gold_col[cfg$field == "sample_size"]))
  expect_false(cfg$include[cfg$field == "sample_size"])
  # A field the schema does not have, added by the mapping; the AI has no
  # column for it, so it is not scored.
  expect_equal(cfg$gold_col[cfg$field == "habitat"], "habitat_notes")
  expect_false(cfg$include[cfg$field == "habitat"])
  expect_equal(cfg$comparator[cfg$field == "location_country"], "fuzzy")
  expect_equal(cfg$threshold[cfg$field == "location_country"], 0.7)
  expect_equal(cfg$field[cfg$linkage], "interaction_type")

  cfg <- configure_fields(schema, ai, gold, keys,
                          fields = c("interaction_type", "location_country"))
  expect_setequal(cfg$field[cfg$include], c("interaction_type", "location_country"))
  expect_equal(cfg$field[cfg$linkage], "interaction_type")

  expect_error(configure_fields(schema, ai, gold, keys,
                                comparator_config = data.frame(field = "x", comparator = "magic")),
               "Unknown comparator")
  expect_error(configure_fields(schema, ai, gold, keys, fields = "nonesuch"), "nonesuch")
})

test_that("a column mapped explicitly is not also left on a suggested field", {
  schema <- read_schema(fixture("schema.json"))
  ai <- read_table_any(fixture("ai_records.csv"))
  gold <- read_table_any(fixture("gold_records.csv"))
  cfg <- configure_fields(schema, ai, gold, list(gold = paper_key("doi")),
                          mapping = list(gold = c(sample_size = "year_observed")))
  expect_equal(cfg$gold_col[cfg$field == "sample_size"], "year_observed")
  expect_true(is.na(cfg$gold_col[cfg$field == "year_observed"]))
})

test_that("a saved configuration without column mappings keeps the suggestions", {
  schema <- read_schema(fixture("schema.json"))
  ai <- read_table_any(fixture("ai_records.csv"))
  gold <- read_table_any(fixture("gold_records.csv"))
  saved <- default_comparator_config(schema)
  saved$ai_col <- NA_character_
  saved$gold_col <- NA_character_
  cfg <- configure_fields(schema, ai, gold, list(ai = paper_key("doi")),
                          comparator_config = saved)
  expect_equal(cfg$ai_col, cfg$field)
})

test_that("normalisers change what is compared, not what is shown", {
  to_code <- function(x) ifelse(x == "United States", "USA", x)
  run <- do.call(setup_evaluation, ex_args(
    normalizers = list(location_country = to_code)))
  cells <- run$scored$cells
  us <- cells[cells$field == "location_country" &
                cells$ai_original %in% "United States", ]
  expect_true(nrow(us) > 0L)
  expect_true(all(us$ai_value == "USA"))
  expect_true(all(us$state[us$gold_value %in% "USA"] == "agree"))
  expect_equal(run$config$normalizer[run$config$field == "location_country"], "custom")

  # The heatmap and the export show what each side said.
  grid <- record_field_outcomes(cells, us$paper[[1L]])
  expect_true("United States" %in% grid$ai_original)
  p <- plot_record_heatmap(grid[grid$ai_original %in% "United States", ])
  expect_match(p$data$text[[1L]], "United States (compared as USA)", fixed = TRUE)
  wide <- aligned_table(cells, run$scored$pairs)
  expect_true("United States" %in% wide$ai_location_country)

  # Switched off, the normaliser does nothing.
  expect_error(do.call(setup_evaluation, ex_args(skip = "normize")), "normize")
  off <- do.call(setup_evaluation, ex_args(
    normalizers = list(location_country = to_code), skip = "normalize"))
  cells <- off$scored$cells
  expect_true(all(cells$ai_value == cells$ai_original, na.rm = TRUE))
})

test_that("a normaliser must return one value per input", {
  run <- do.call(setup_evaluation, ex_args(
    normalizers = list(location_country = function(x) x[1])))
  expect_equal(run$stage, "fields")
  expect_match(run$message, "returned 1 values")
  expect_error(do.call(setup_evaluation, ex_args(normalizers = list(function(x) x))),
               "named list of functions")
})

test_that("the schema and granularity checks can be skipped", {
  run <- do.call(setup_evaluation, ex_args(skip = c("conformance", "granularity")))
  expect_null(run$scored$conformance)
  expect_null(run$scored$collapses)
  run <- do.call(setup_evaluation, ex_args())
  expect_true(nrow(run$scored$conformance) > 0L)
})

test_that("evaluate_extraction takes the same setup inputs and fails loudly", {
  res <- evaluate_extraction(
    fixture("ai_records.csv"), fixture("gold_records.csv"), fixture("schema.json"),
    fields = c("bat_species_scientific_name", "interaction_type", "sample_size"),
    skip = "conformance"
  )
  expect_setequal(unique(res$cells$field),
                  c("bat_species_scientific_name", "interaction_type", "sample_size"))
  expect_null(res$conformance)
  expect_s3_class(res$findings, "tbl_df")
  expect_error(
    evaluate_extraction(fixture("ai_records.csv"), fixture("gold_records.csv"),
                        fixture("schema.json"), linkage_fields = character(0)),
    "identity column"
  )
})
