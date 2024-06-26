test_that("basic case works", {
  skip_on_cran()
  dates <- sample(seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = 1), 50)
  ans <- test_if_dates(dates, 3, 30, 365)
  expect_equal(if_dates(dates, 3, 30, 365), ans)
})

test_that("dup.rm works", {
  dates <- c(rep(as.Date("2023-12-01"), 3), as.Date("2023-12-08"), as.Date("2023-12-16"))
  expect_true(if_dates(dates, 5, within = 30, dup.rm = FALSE))
  expect_false(if_dates(dates, 5, within = 30, dup.rm = TRUE))
})

test_that("input checks work", {
  dates <- c(as.Date("2023-12-08"), as.Date("2023-12-16"))
  expect_error(if_dates(dates, n = 3), "both be NULL")
  expect_error(if_dates(dates, n = 2, apart = 2.5), "is.wholenumber")
  expect_error(if_dates(dates, n = 2, apart = 7, within = 2.5), "is.wholenumber")
  expect_error(if_dates(dates, n = 2, apart = 1.5, within = 2.5), "is.wholenumber")
  expect_error(if_dates(dates, n = 2, apart = 7, within = 5), "impossible")
  # must be false if dates is shorter than n
  expect_false(if_dates(dates, n = 5, apart = 3))
})

test_that("apart only works", {
  skip_on_cran()
  dates <- sample(seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = 1), 50)
  ans <- test_if_dates(dates, 3, apart = 30)
  expect_equal(if_dates(dates, 3, apart = 30), ans)
})

test_that("within only works", {
  skip_on_cran()
  dates <- sample(seq(as.Date("2015-01-01"), as.Date("2023-12-31"), by = 1), 50)
  ans <- test_if_dates(dates, 3, within = 365)
  expect_equal(if_dates(dates, 3, within = 365), ans)
})

test_that("straight false due to no gap < within works", {
  dates <- c(as.Date("2023-01-01"), as.Date("2023-03-01"), as.Date("2023-05-01"))
  expect_false(if_dates(dates, 2, apart = 7, within = 30))
})

test_that("detail works", {
  x <- as.Date(c("2010-01-01", "2012-05-03", "2015-01-07", "2015-02-01", "2017-02-08", "2017-05-07"))
  ans <- c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  w <- 365
  out <- if_dates(x, n = 2, within = w, detail = TRUE)
  expect_equal(out, ans)
})

test_that("detail and dup.rm edge case", {
  x <- as.Date(c("2010-01-01", "2010-01-01", "2012-05-03", "2012-05-03", "2015-01-07", "2015-02-01", "2017-02-08", "2017-05-07"))
  ans_n2_dupT <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  w <- 365
  out <- if_dates(x, n = 2, within = w, detail = TRUE)
  expect_equal(out, ans_n2_dupT)
  ans_n2_dupF <- c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE)
  out <- if_dates(x, n = 2, within = w, detail = TRUE, dup.rm = FALSE)
  expect_equal(out, ans_n2_dupF)
  x <- as.Date(c("2010-01-01", "2010-01-01", "2010-05-03", "2010-05-03", "2015-01-07", "2017-02-01", "2017-02-08", "2017-05-07"))
  ans_n3_dupT <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  out <- if_dates(x, n = 3, within = w, detail = TRUE, dup.rm = TRUE)
  expect_equal(out, ans_n3_dupT)
  ans_n3_dupF <- c(TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  out <- if_dates(x, n = 3, within = w, detail = TRUE, dup.rm = FALSE)
  expect_equal(out, ans_n3_dupF)
})

test_that("align works", {
  x <- as.Date(c("2010-01-01", "2012-05-03", "2015-01-07", "2015-02-01", "2017-02-08", "2017-05-07"))
  ans <- c(FALSE, FALSE, FALSE, TRUE, FALSE, TRUE)
  w <- 365
  out <- if_dates(x, n = 2, within = w, detail = TRUE, align = "right")
  expect_equal(out, ans)
})

test_that("align with apart works", {
  x <- as.Date(c("2010-01-01", "2012-05-03", "2015-01-07", "2015-02-01", "2017-02-08", "2017-05-07"))
  ans <- c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  w <- 365
  a <- 60
  out <- if_dates(x, n = 2, apart = a, within = w, detail = TRUE, align = "right")
  expect_equal(out, ans)
})

test_that("sort back works", {
  x <- as.Date(c("2017-05-07", "2015-02-01", "2010-01-01", "2012-05-03", "2015-01-07",  "2017-02-08"))
  ans <- c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  w <- 365
  out <- if_dates(x, n = 2, within = w, detail = TRUE, align = "right")
  expect_equal(out, ans)
})

