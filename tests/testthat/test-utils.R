test_that("is_blank treats NA, empty and whitespace alike", {
  expect_equal(is_blank(c(NA, "", "  ", "a", "0")),
               c(TRUE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(is_blank(list(NULL, character(0), NA, "x")),
               c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(is_blank(NULL), logical(0))
})

test_that("is_blank does not treat zero or FALSE as blank", {
  expect_false(is_blank(0))
  expect_false(is_blank(FALSE))
})

test_that("as_set splits the delimiters records actually use", {
  expect_equal(as_set("a; b|c"), c("a", "b", "c"))
  expect_equal(as_set("a, b, c"), c("a", "b", "c"))
  expect_equal(as_set(list(c("a", "b"))), c("a", "b"))
  expect_equal(as_set(NA), character(0))
})

test_that("as_set does not split inside a single unpunctuated value", {
  expect_equal(as_set("Myotis lucifugus"), "Myotis lucifugus")
})

test_that("squash_name collapses naming conventions onto one form", {
  expect_equal(squash_name(c("Bat Species", "bat_species", "batSpecies")),
               rep("batspecies", 3))
})

test_that("pair_key is vectorised, so batch cost estimates are right", {
  expect_length(pair_key("f", c("a", "b"), c("x", "y")), 2L)
  expect_false(identical(pair_key("f", "a", "b"), pair_key("f", "b", "a")))
})
