library(geocacheR)
library(testthat)
library(dplyr)
library(tidyr)

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


context("analyse_frequency")

test_that("analyse_frequency outputs equal to expected values", {
  expect_equal(
    analyse_frequency("abcxyz!") %>% select(-input_string) %>% rowSums(),
    7L)
  expect_equal(
    analyse_frequency("abcxyz!")[, c("D", "B", "X")],
    data.frame(D=0L, B=1L, X=1L))
  expect_equal(
    analyse_frequency("") %>% select(-input_string) %>% rowSums(),
    0L)
})
test_that("vectorised inputs", {
  expect_equal(
    analyse_frequency(c("hello", "world!")) %>% select(-input_string) %>% rowSums(),
    c(5L, 6L))
  expect_equal(
    analyse_frequency(c("hello", "world!"))$L,
    c(2L, 1L))
  expect_equal(
    analyse_frequency(c("hello", "world!"))$`!`,
    c(0L, 1L))
})
test_that("unicode inputs", {
  expect_equal(
    analyse_frequency(c("\u2e18Hola\u203d")) %>% select(-!!LETTERS, -input_string) %>% unlist(use.names = FALSE),
    c(1L, 1L))
  expect_equal(
    analyse_frequency(c("brødløs")) %>% select(-input_string) %>% apply(1, max),
    2L)
})
test_that("dataframe-unfriendly inputs", {
  expect_equal(
    analyse_frequency("\t\n\r \ud\u6") %>% select(-input_string) %>% rowSums(),
    6L)
})
