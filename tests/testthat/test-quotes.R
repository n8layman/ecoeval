test_that("a JSON array of quotes is split into its sentences", {
  expect_equal(quote_sentences('["Bats roosted.", "Two were \\"caught\\".\\nLater."]'),
               c("Bats roosted.", "Two were \"caught\".\nLater."))
  expect_equal(quote_sentences("Bats roosted in the barn."), "Bats roosted in the barn.")
  expect_equal(quote_sentences(c("a", " ", NA)), "a")
  expect_equal(quote_sentences("[]"), character(0))
  expect_equal(quote_sentences(NA_character_), character(0))
  # Brackets that are not JSON are text.
  expect_equal(quote_sentences("[sic] bats roosted"), "[sic] bats roosted")
  expect_equal(quote_sentences(list(c("a", "b"))), c("a", "b"))
})
