library(geocacheR)
library(testthat)
library(tidyverse)

context("rot")

test_that("rot inputs and outputs are of equal length", {
  expect_equal(nchar(rot("a")), 1)
  expect_equal(nchar(rot("ab")), 2)
  expect_equal(nchar(rot("a98")), 3)
})
test_that("rot output matches expected values", {
  expect_equal(rot("a"), "n")
  expect_equal(rot("abc", -1), "zab")
  expect_equal(rot("Gl\u00e6delig!",
                   alphabet=list(lw=c(letters, "\u00e6", "\u00f8", "\u00e5"),
                                 up=c(LETTERS, "\u00c6", "\u00d8", "\u00c5"))),
               "Tykqryvt!")
  expect_equal(
    rot("Abc4567xyZ", 3, alphabet=list(lw=c(letters, 0:4), up=c(LETTERS, 5:9))),
    "Defc89A017"
  )
})
