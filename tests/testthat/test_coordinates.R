library(geocacheR)
library(testthat)
library(stringr)
library(magrittr)

context("parseCoordinates")

test_that("DMms in a single string", {
  expect_equal(parseCoordinates("N55 55.555 W003 14.159"),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates("N 55 55.555 W003 14.159"),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates("N55 55.555 W 003 14.159"),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates("N 55 55.555 W 003 14.159"),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates("N 5 5.555 W3 9.159"),
               c(n=5.09258333, e=-3.15265))
  expect_equal(parseCoordinates("55 55.555 003 14.159"),
               c(n=55.9259167, e=3.235983))
  expect_equal(parseCoordinates("N55°55.555 W003°14.159"),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates("N55° 55.555' W003° 14.159'"),
               c(n=55.9259167, e=-3.235983))
})

test_that("DMms in two strings", {
  expect_equal(parseCoordinates(c("N55 55.555", "W003 14.159")),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates(c("N 55 55.555", "W003 14.159")),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates(c("N55 55.555", "W 003 14.159")),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates(c("N 55 55.555", "W 003 14.159")),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates(c("N 5 5.555", "W3 9.159")),
               c(n=5.09258333, e=-3.15265))
  expect_equal(parseCoordinates(c("55 55.555", "003 14.159")),
               c(n=55.9259167, e=3.235983))
  expect_equal(parseCoordinates(c("N55°55.555", "W003°14.159")),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates(c("N55° 55.555'", "W003° 14.159'")),
               c(n=55.9259167, e=-3.235983))
})

test_that("Dds in a single string", {
  expect_equal(parseCoordinates("N55.9259167 W3.235983"),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates("N55 W3.0"), c(n=55, e=-3))
  expect_equal(parseCoordinates("N55.9259167° W3.235983°"),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates("N55 W3.0°"), c(n=55, e=-3))
})

test_that("Dds in two strings", {
  expect_equal(parseCoordinates(c("N55.9259167", "W3.235983")),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates(c("N55", "W3.0")), c(n=55, e=-3))
  expect_equal(parseCoordinates(c("N55.9259167°", "W3.235983°")),
               c(n=55.9259167, e=-3.235983))
  expect_equal(parseCoordinates(c("N55°", "W3.0°")), c(n=55, e=-3))
})

test_that("DMS in a single string", {
  expect_equal(parseCoordinates("N55 55 33.3\" W003° 14' 9.54"),
               c(n=55.925917, e=-3.235983))
})

test_that("DMS in two strings", {
  expect_equal(parseCoordinates(c("N55 55 33.3\"", "W003° 14' 9.54")),
               c(n=55.925917, e=-3.235983))
})

context("expressCoordinates")
test_that("Geocaching style", {
  expect_equal(expressCoordinates(c(n=58.560277, e=-3.748217)),
               "N58 33.617 W003 44.893")
})
