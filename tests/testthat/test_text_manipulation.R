library(geocacheR)
library(testthat)
library(dplyr)

context("rot")

test_that("rot inputs and outputs are of equal length", {
  expect_equal(nchar(rot("a")), 1)
  expect_equal(nchar(rot("ab")), 2)
  expect_equal(nchar(rot("a98")), 3)
})
test_that("rot output matches expected values", {
  expect_equal(rot("a"), "n")
  expect_equal(rot("abc", -1), "zab")
  expect_equal(rot("6acb9296d8d3758f6817558a07b95d85", 1,
               alphabet=list(lw=c(0:9, letters[1:6]),
                             up=c(0:9, LETTERS[1:6]))),
               "7bdca3a7e9e486907928669b18ca6e96")
  expect_equal(
    rot("Abc4567xyZ", 3, alphabet=list(lw=c(letters, 0:4), up=c(LETTERS, 5:9))),
    "Defc89A017"
  )
})

context("rot_all")

test_that("rot_all outputs are of the same length as the alphabet", {
  expect_equal(length(rot_all("a")), 26)
  expect_equal(length(rot_all("a\u00e6")), 26)
  expect_equal(length(rot_all("a", alphabet=letters[1:4])), 4)
})
test_that("rot_all outputs are as expected", {
  expect_equal(rot_all("a", alphabet=letters[1:4]), c("b", "c", "d", "a"))
})

context("vigenere")

test_that("vigenere outputs equal to expected values", {
  expect_equal(
    vigenere("mlw jnmud uvgpg jgq", key = "te s1t"),
    "the quick brown fox")
  expect_equal(
    vigenere("the 'quick' brown f4ox", key = "te s1t", decrypt = FALSE),
    "mlw 'jnmud' uvgpg j4gq")
})
