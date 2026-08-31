test_that("every input format the app accepts round-trips", {
  csv <- read_table_any(fixture("ai_records.csv"))
  db <- read_table_any(fixture("ai_records.db"), "records")
  expect_equal(nrow(csv), nrow(db))
  expect_true(all(names(csv) %in% names(db)))
})

test_that("an unreadable path fails with a clear message", {
  expect_error(read_table_any("no/such/file.csv"), "File not found")
  tmp <- tempfile(fileext = ".docx"); file.create(tmp)
  expect_error(read_table_any(tmp), "Unsupported file type")
})

test_that("the largest table is the record table in an ecoextract database", {
  tabs <- list_db_tables(fixture("ai_records.db"))
  expect_equal(tabs$table[[1L]], "records")
  expect_false(is.unsorted(rev(tabs$n_rows)))   # largest first
})

test_that("naming a table that is not there says which ones are", {
  expect_error(read_table_any(fixture("ai_records.db"), "nope"), "Available")
})

test_that("the AI paper list comes free from the documents table", {
  docs <- read_ecoextract_documents(fixture("ai_records.db"))
  expect_true(!is.null(docs))
  # It knows about papers that produced no records -- which is the whole point.
  recs <- read_table_any(fixture("ai_records.db"), "records")
  expect_true(length(setdiff(docs$doi, recs$doi)) > 0L)
})

test_that("DOI is preferred as the paper identifier", {
  expect_equal(suggest_paper_column(tibble::tibble(ID = 1, DOI = "x", Title = "y")),
               "DOI")
  expect_equal(suggest_paper_column(tibble::tibble(a = 1, paper_id = "x")),
               "paper_id")
  expect_true(is.na(suggest_paper_column(tibble::tibble(a = 1, b = 2))))
})

test_that("mapping suggestions survive a change of naming convention", {
  m <- suggest_mapping(c("Bat Species", "Interaction Type", "Habitat Notes"),
                       c("bat_species", "interaction_type", "location"))
  expect_equal(m$to[m$from == "Bat Species"], "bat_species")
  expect_equal(m$basis[m$from == "Bat Species"], "squashed")
  expect_true(is.na(m$to[m$from == "Habitat Notes"]))
})

test_that("mapping is one-to-one -- a target is never suggested twice", {
  m <- suggest_mapping(c("species", "species_name"), c("species"))
  expect_equal(sum(!is.na(m$to)), 1L)
})

test_that("prepare_records keys rows and canonicalises the paper column", {
  raw <- read_table_any(fixture("ai_records.csv"))
  recs <- prepare_records(raw, "doi", prefix = "a")
  expect_true(all(c(".rid", ".paper") %in% names(recs)))
  expect_equal(anyDuplicated(recs$.rid), 0L)
  expect_false("doi" %in% names(recs))
})

test_that("record keys carry a prefix, so AI and gold keys cannot collide", {
  raw <- read_table_any(fixture("ai_records.csv"))
  a <- prepare_records(raw, "doi", prefix = "a")
  g <- prepare_records(raw, "doi", prefix = "g")
  expect_length(intersect(a$.rid, g$.rid), 0L)
})

test_that("a paper list without an identifier column is refused", {
  expect_error(prepare_papers(tibble::tibble(a = 1), "doi"),
               class = "ecoeval_error")
})

test_that("scope is the intersection, and the rest is excluded not penalised", {
  fx <- fixture_run()
  expect_equal(length(fx$scope$papers), 10L)
  # P11: the AI processed it and found nothing.  P12: a human read it and
  # found nothing.  Each is visible only because that side supplied a list.
  expect_equal(fx$scope$ai_only, "10.1000/p11")
  expect_equal(fx$scope$gold_only, "10.1000/p12")
})

test_that("without a paper list, scope falls back to the papers in the records", {
  fx <- fixture_run()
  no_list <- compute_scope(paper_set(fx$ai), paper_set(fx$gold))
  expect_false("10.1000/p11" %in% no_list$papers)
  expect_false("10.1000/p07" %in% no_list$gold_only)
})

test_that("the coverage warning fires only on a lopsided pairing", {
  fx <- fixture_run()
  expect_null(coverage_warning(TRUE, TRUE, fx$scope))
  expect_null(coverage_warning(FALSE, FALSE, fx$scope))
  expect_match(coverage_warning(FALSE, TRUE, fx$scope), "over-extraction")
  expect_match(coverage_warning(TRUE, FALSE, fx$scope), "under-extraction")
})
