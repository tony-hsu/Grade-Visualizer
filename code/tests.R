library(testthat)

context("Get statistics functions")

test_that("remove missing works", {
  a = c(NA, 1, 2, 3)
  b = c(NA, 1:8)
  c = c(1:5, NA)
  d = c(1:2, NA)
  expect_equal(remove_missing(a), c(1,2,3))
  expect_equal(remove_missing(b), 1:8)
  expect_equal(remove_missing(c), 1:5)
  expect_equal(remove_missing(d), 1:2)
}
          )


context("Get statistics functions")

test_that("get missing works", {
  a = c(NA, 1, 2, 3)
  b = c(NA, 1:8)
  c = c(2,5,3,-1)
  d = c(2,3,5,0.5)
  expect_equal(get_minimum(a, TRUE), 1)
  expect_equal(get_minimum(b, TRUE), 1)
  expect_equal(get_minimum(c, TRUE), -1)
  expect_equal(get_minimum(d, TRUE), 0.5)
}
)

context("Get statistics functions")

test_that("get missing works", {
  a = c(NA, 1, 2, 3)
  b = c(NA, 1:8)
  c = c(2,5,3,-1)
  d = c(2,3,5,0.5)
  expect_equal(get_maximum(a, TRUE), 3)
  expect_equal(get_maximum(b, TRUE), 8)
  expect_equal(get_maximum(c, TRUE), 5)
  expect_equal(get_maximum(d, TRUE), 5)
}
)

context("Get statistics functions")

test_that("get missing works", {
  a = c(NA, 1, 2, 3)
  b = c(NA, 1:8)
  c = c(2,5,3,-1)
  d = c(2,3,5,0.5)
  expect_equal(get_range(a, TRUE), 2)
  expect_equal(get_range(b, TRUE), 7)
  expect_equal(get_range(c, TRUE), 6)
  expect_equal(get_range(d, TRUE), 4.5)
}
)

context("Get statistics functions")

test_that("get missing works", {
  a = c(NA, 1, 2, 3)
  b = c(NA, 1:8)
  c = c(2,5,3,-1)
  d = c(2,3,5,0.5)
  expect_equal(get_percentile10(a, TRUE), quantile(c(1,2,3), 0.1)[[1]])
  expect_equal(get_percentile10(b, TRUE), quantile(c(1:8), 0.1)[[1]])
  expect_equal(get_percentile10(c, TRUE), quantile(c, 0.1)[[1]])
  expect_equal(get_percentile10(d, TRUE), quantile(d, 0.1)[[1]])
}
)

context("function drop_lowest")
test_that("drop lowest value as expected", {
  g = c(2, 5, 6, 7, 23)
  b = c(14, 10, 3, 50)
  e = c(1,2,3)
  f = c(21, 31, 30, 100, 33, 40, 48)
  expect_equal(drop_lowest(g), c(5, 6, 7, 23))
  expect_equal(drop_lowest(b), c(14, 10, 50))
  expect_equal(drop_lowest(e), c(2,3))
  expect_equal(drop_lowest(f), c(31, 30, 100, 33, 40, 48))
})


context("function rescale100")
test_that("rescaled vector from 0 to 100 as expected", {
  g = c(17, 39, 42, 10, 23)
  f = c(151, 139, 199, 241, 136)
  a = c(3, 10, 6, 18, 13)
  c = c(6, 11, 27, 21, 38)
  expect_equal(rescale100(a, xmin = 0, xmax = 20), c(15, 50, 30, 90, 65))
  expect_equal(rescale100(c, xmin = 0, xmax = 40), c(15, 27.5, 67.5, 52.5, 95))
  expect_equal(rescale100(g, xmin = 0, xmax = 50), c(34, 78, 84, 20, 46))
  expect_equal(rescale100(f, xmin = 100, xmax = 250), c(34, 26, 66, 94, 24))
})


context("function score_homework")
test_that("return the average of the homework scores as expected", {
  e = c(70, 48, 88, 92, 67)
  y = c(57, 90, 64, 32, 86, 25)
  b = c(19, 58, 84, 72, 69)
  d = c(43, 75, 83, 20, 70, 64, 100)
  expect_equal(score_homework(e, drop = TRUE), 79.25)
  expect_equal(score_homework(b, drop = FALSE), 60.4)
  expect_equal(score_homework(d, drop = TRUE), 72.5)
  expect_equal(score_homework(y, drop = FALSE), 59)
})


context("function score_quiz")
test_that("return the average of the quiz scores as expected", {
  c = c(80, 63, 52, 98, 65)
  b = c(67, 49, 87, 90, 75)
  d = c(19, 59, 84, 72, 69, 99)
  e = c(57, 90, 64, 38, 86, 25)
  expect_equal(score_quiz(c, drop = TRUE), 76.5)
  expect_equal(score_quiz(b, drop = FALSE), 73.6)
  expect_equal(score_quiz(d, drop = TRUE), 76.6)
  expect_equal(score_quiz(e, drop = FALSE), 60)
})


context("function score_lab")
test_that("return lab score as expected", {
  expect_equal(score_lab(12), 100)
  expect_equal(score_lab(2), 0)
  expect_equal(score_lab(7), 20)
  expect_equal(score_lab(9), 60)
})


context('get_percentile90')

test_that('get_percentile90() works', {
  v = c(-1, 5, 3)
  e = c(1, 2, NA, 6)
  r = c(1, 2, 3, NA)
  o = 0:100
  expect_equal(get_percentile90(v, na.rm = FALSE), 4.6)
  expect_equal(get_percentile90(e, na.rm = FALSE), NA)
  expect_equal(get_percentile90(r, na.rm = TRUE), 2.8)
  expect_equal(get_percentile90(o, na.rm = FALSE), 90)
})

context('get_quartile1')

test_that('get_quartile1() works', {
  v = c(-1, 5, 3)
  e = c(1, 2, NA, 6)
  r = c(1, 2, 3, NA)
  o = 0:100
  expect_equal(get_quartile1(v, na.rm = FALSE), 1)
  expect_equal(get_quartile1(e, na.rm = FALSE), NA)
  expect_equal(get_quartile1(r, na.rm = TRUE), 1.5)
  expect_equal(get_quartile1(o, na.rm = FALSE), 25)
})

context('get_quartile3')

test_that('get_quartile3() works', {
  v = c(-1, 5, 3)
  o = 0:100
  e = c(1, 2, NA, 6)
  r = c(1, 2, 3, NA)
  expect_equal(get_quartile3(v, na.rm = FALSE), 4)
  expect_equal(get_quartile3(e, na.rm = FALSE), NA)
  expect_equal(get_quartile3(r, na.rm = TRUE), 2.5)
  expect_equal(get_quartile3(o, na.rm = FALSE), 75)
})

context('get_median')

test_that('get_median() works', {
  v = c(-1, 5, 3)
  e = c(1, 2, NA, 6)
  r = c(1, 2, 3, NA)
  o = 0:100
  expect_equal(get_median(v, na.rm = FALSE), 3)
  expect_equal(get_median(e, na.rm = FALSE), NA)
  expect_equal(get_median(r, na.rm = TRUE), 2)
  expect_equal(get_median(o, na.rm = FALSE), 50)
})

context("function get_average()")
test_that("calculated average as expected", {
  a = c(NA,15,4,8,20)
  b = c(1:2)
  c = c(1:10, NA)
  d = c(NA,NA,9:14)
  expect_equal(get_average(a,na.rm = TRUE),11.75)
  expect_equal(get_average(b, FALSE),1.5)
  expect_equal(get_average(c,na.rm = FALSE),NA)
  expect_equal(get_average(d, TRUE),11.5)
}
)

context("unit test of calculated standard deviation as expected")
test_that("calculated standard deviation as expected", {
  a = c(NA,15,4,8,20)
  b = c(1:2)
  c = c(1:10, NA)
  d = c(NA,NA,9:14)
  expect_equal(get_stdev(a,na.rm = TRUE),sd(c(15,4,8,20)))
  expect_equal(get_stdev(b, FALSE), sd(1:2))
  expect_equal(get_stdev(c,na.rm = FALSE),NA)
  expect_equal(get_stdev(d, TRUE),sd(9:14))
}
)

context("unit test of correctly counts missing values")
test_that("correctly counts missing values", {
  a = c(NA,15,4,8,20)
  b = c(1:2)
  c = c(1:10, NA)
  d = c(NA,NA,9:14)
  expect_equal(count_missing(a),1)
  expect_equal(count_missing(b),0)
  expect_equal(count_missing(c),1)
  expect_equal(count_missing(d),2)
}
)
context("test for summary stats")
test_that("correctly outputs all of the summary statistics", {
  a = c(NA,15,4,8,20)
  b = c(NA,NA,NA, 1:5)
  c = c(1:5)
  d = c(NA,NA,9:14)
  expect_equal(summary_stats(a),list("minimum" = 4, "percent10" = 5.2, "quartile1" = 7,
                                     "median" = 11.5, "mean" = 11.75, "quartile3" = 16.25, "percent90"= 18.5,
                                     "maximum"= 20, "range"= 16, "stdev"= sd(c(15,4,8,20)), "missing"= 1))
  expect_equal(summary_stats(b),list("minimum" = 1, "percent10" = 1.4, "quartile1" = 2,
                                     "median" = 3, "mean" = 3, "quartile3" = 4, "percent90"= 4.6,
                                     "maximum"= 5, "range"= 4, "stdev"= sd(c(1:5)), "missing"= 3))
  expect_equal(summary_stats(c),list("minimum" = 1, "percent10" = 1.4, "quartile1" = 2,
                                     "median" = 3, "mean" = 3, "quartile3" = 4, "percent90"= 4.6,
                                     "maximum"= 5, "range"= 4, "stdev"= sd(c(1:5)), "missing"= 0))
  expect_equal(summary_stats(d),list("minimum" = 9, "percent10" = 9.5, "quartile1" = 10.25,
                                     "median" = 11.5, "mean" = 11.5, "quartile3" = 12.75, "percent90"= 13.5,
                                     "maximum"= 14, "range"= 5, "stdev"= sd(c(9:14)), "missing"= 2))
}
)




