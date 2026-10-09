# What identifies a paper is detected, not asked for: DOI, then file name,
# then title, then first author + year. These tests pin the priority order,
# the multi-column case, and the per-role normalisation that makes two
# sources' spellings of the same paper agree.

test_that("the priority order holds, and better identifiers win", {
  df <- tibble::tibble(Filename = "a.pdf", DOI = "10.1/x", Title = "T",
                       Author = "Smith, J.", Year = 2019)
  expect_equal(suggest_paper_key(df)$strategy, "doi")
  expect_equal(suggest_paper_key(df)$columns, "DOI")

  no_doi <- df[setdiff(names(df), "DOI")]
  expect_equal(suggest_paper_key(no_doi)$strategy, "filename")

  no_file <- no_doi[setdiff(names(no_doi), "Filename")]
  expect_equal(suggest_paper_key(no_file)$strategy, "title")

  by_author <- no_file[setdiff(names(no_file), "Title")]
  expect_equal(suggest_paper_key(by_author)$strategy, "author_year")
})

test_that("an identifier can span more than one column", {
  key <- suggest_paper_key(tibble::tibble(first_author = "Smith, J.", year = 2019))
  expect_equal(key$columns, c("first_author", "year"))
  expect_equal(key$roles, c("author", "year"))
  expect_equal(paper_key_values(tibble::tibble(first_author = "Smith, J.",
                                               year = 2019), key),
               "smith|2019")
})

test_that("a table with nothing identifying in it gets no key", {
  expect_null(suggest_paper_key(tibble::tibble(a = 1, b = 2)))
  expect_length(paper_key_candidates(tibble::tibble(a = 1)), 0L)
  expect_true(is.na(paper_key_values(tibble::tibble(a = 1))))
})

test_that("every identifier the table could use is offered, best first", {
  cand <- paper_key_candidates(tibble::tibble(doi = "x", title = "y",
                                              author = "z", year = 2000))
  expect_equal(names(cand), c("doi", "title", "author_year"))
})

test_that("a DOI is the same paper however it was written", {
  df <- tibble::tibble(doi = c("https://doi.org/10.1000/P01", "doi: 10.1000/p01",
                               "10.1000/p01/", "  10.1000/p01  "))
  expect_length(unique(paper_key_values(df, "doi")), 1L)
})

test_that("a file name loses its directory, extension, and separators", {
  df <- tibble::tibble(filename = c("/papers/Smith_2019.pdf", "smith 2019.PDF"))
  expect_length(unique(paper_key_values(df, "filename")), 1L)
})

test_that("a title survives a change of punctuation", {
  df <- tibble::tibble(title = c("Attic roosting in Eptesicus fuscus",
                                 "Attic Roosting in  Eptesicus fuscus."))
  expect_length(unique(paper_key_values(df, "title")), 1L)
})

test_that("the first author's surname is read out of any citation style", {
  df <- tibble::tibble(
    author = c("Smith, J.", "J. Smith", "Jane Smith", "Smith et al.",
               "Smith, J. and Jones, K.", "Smith, J.; Jones, K."),
    year = 2019
  )
  expect_length(unique(paper_key_values(df, c("author", "year"))), 1L)
})

test_that("a row missing any part of the key has no key at all", {
  # Half an identifier would link to the wrong paper, which is worse than
  # not linking.
  df <- tibble::tibble(author = c("Smith", NA, "Jones"), year = c(2019, 2019, NA))
  expect_equal(is.na(paper_key_values(df, c("author", "year"))),
               c(FALSE, TRUE, TRUE))
})

test_that("a key names itself in a way the app can print", {
  expect_equal(format(paper_key(c("first_author", "year"))),
               "First author + year (first_author + year)")
  expect_equal(paper_key(c("a", "b"))$strategy, "custom")
})

test_that("metadata for paper alignment comes from the same role detection", {
  meta <- paper_metadata_columns(tibble::tibble(DOI = "x", Title = "y",
                                                Year = 2020))
  expect_equal(meta, c(title = "Title", year = "Year"))
  expect_length(paper_metadata_columns(tibble::tibble(doi = "x")), 0L)
})
