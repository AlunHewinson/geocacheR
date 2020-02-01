## test conversions

context("Scrabble scores")

test_that("words are scored correctly", {
  expect_equal(Scrabble_score(""), 0)
  expect_equal(Scrabble_score("correct"), 11)
  expect_equal(Scrabble_score("horse"), 8)
  expect_equal(Scrabble_score("bat_ery"), 11)
  expect_equal(Scrabble_score("staple"), 8)
})
