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

context("convert_gc")

test_that("id to GC conforms with known values", {
  expect_warning(convert_gc(0))
  expect_warning(convert_gc(1.5))
  expect_equal(convert_gc(1), list(list(code="GC1", id="1")))
  expect_equal(convert_gc("16"), list(list(code="GC10", id="16")))
  expect_equal(convert_gc("65535"), list(list(code="GCFFFF", id="65535")))
  expect_equal(convert_gc("65536"), list(list(code="GCG000", id="65536")))
  expect_equal(convert_gc(1000000L), list(list(code="GC1GBC0", id="1000000")))
  expect_equal(convert_gc("GC3NWRQ"), list(list(code="GC3NWRQ", id="3011768")))
  expect_equal(convert_gc("GCZZZZZ"), list(list(code="GCZZZZZ", id="28218030")))
  expect_equal(convert_gc("GCQWERTY"), list(list(code="GCQWERTY", id="683435362")))
  expect_warning(convert_gc("GCZZZ"))
  expect_warning(convert_gc("GCF02G"))
  expect_warning(convert_gc("TB5C1YB"))
  expect_warning(convert_gc("GC11L1"))
})
