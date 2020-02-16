## test conversions

context("Scrabble scores")

test_that("words are scored correctly", {
  expect_equal(Scrabble_score(""), 0)
  expect_equal(Scrabble_score("correct"), 11)
  expect_equal(Scrabble_score("horse"), 8)
  expect_equal(Scrabble_score("bat_ery"), 11)
  expect_equal(Scrabble_score("staple"), 8)
})

context("tvaersum")

test_that("tvaersums output known values", {
  expect_equal(tvaersum(0), 0L)
  expect_equal(tvaersum("0"), 0L)
  expect_equal(tvaersum(9), 9L)
  expect_equal(tvaersum("9"), 9L)
  expect_equal(tvaersum(5), 5L)
  expect_equal(tvaersum(5L), 5L)
  expect_equal(tvaersum(213956), 8L)
  expect_equal(tvaersum("999"), 9L)
  expect_warning(tvaersum("12.3"))
  expect_equal(suppressWarnings(tvaersum("12.3")), 6L)
})
