# The source documents: reading the OCR text and finding a cell's values in it.

test_that("the OCR text comes from the database, without the page images", {
  d <- read_ecoextract_texts(fixture("ai_records.db"))
  expect_true(all(c("doi", "document_content", "extraction_reasoning") %in% names(d)))
  expect_false("ocr_images" %in% names(d))
  expect_equal(nrow(d), 11L)

  # A database whose documents table has no text has nothing to show.
  path <- withr::local_tempfile(fileext = ".db")
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbWriteTable(con, "documents", data.frame(doi = "x"))
  DBI::dbDisconnect(con)
  expect_null(read_ecoextract_texts(path))
})

test_that("documents are keyed like the AI records", {
  d <- read_ecoextract_texts(fixture("ai_records.db"))
  dt <- document_texts(d, paper_key("doi"))
  expect_equal(nrow(dt), 11L)
  expect_true("10.1000/p03" %in% dt$.paper)
  expect_match(dt$reasoning[[1L]], "extracted one record")
  # Records keyed on a column the documents table lacks: the same kind of
  # identifier is used instead.
  dt <- document_texts(d, paper_key("DOI_link"))
  expect_true("10.1000/p03" %in% dt$.paper)
  expect_equal(nrow(document_texts(NULL, paper_key("doi"))), 0L)
})

test_that("a run on a database carries each paper's text", {
  run <- setup_evaluation(fixture("ai_records.db"), fixture("gold_records.csv"),
                          fixture("schema.json"), ai_table = "records")
  expect_equal(run$stage, "scored")
  expect_true(all(run$scored$cells$paper %in% run$placed$documents$.paper))
  csv <- setup_evaluation(fixture("ai_records.csv"), fixture("gold_records.csv"),
                          fixture("schema.json"))
  expect_equal(nrow(csv$placed$documents), 0L)
})

test_that("passages are found loosely, merged, and marked by side", {
  text <- paste("Intro.", strrep("filler ", 80),
                "We recorded Myotis\nlucifugus roosting in the USA.",
                strrep("filler ", 80), "Code W was used.")
  p <- document_passages(text, c(ai = "myotis lucifugus", gold = "USA",
                                 ai = "W", gold = "Canada"))
  expect_equal(unname(p$found), c(TRUE, TRUE, TRUE, FALSE))
  # The species and the country are close enough to share one passage.
  first <- p$passages[[1L]]
  expect_setequal(first$marks$side, c("ai", "gold"))
  m <- first$marks[first$marks$side == "gold", ]
  expect_equal(substr(first$text, m$start, m$end), "USA")
  expect_length(p$passages, 2L)
})

test_that("a short value only matches as a whole word", {
  p <- document_passages("Western woodland, with W marked.", c(ai = "W"))
  expect_true(p$found[["ai"]])
  m <- p$passages[[1L]]$marks
  expect_equal(substr(p$passages[[1L]]$text, m$start, m$end), "W")
  expect_equal(m$start, 24L)
})

test_that("a quote the OCR did not reproduce whole is found by its opening", {
  text <- "Bridge expansion joints held a maternity colony of little brown bats, as reported."
  quote <- paste("Bridge expansion joints held a maternity colony of little brown",
                 "bats according to the original survey team in the summer.")
  p <- document_passages(text, c(ai_quote = quote))
  expect_true(p$found[["ai_quote"]])
  expect_length(document_passages(NA_character_, c(ai = "x"))$passages, 0L)
  expect_length(document_passages("text", c(ai = NA, gold = ""))$passages, 0L)
})
