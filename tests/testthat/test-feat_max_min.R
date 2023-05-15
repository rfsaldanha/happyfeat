test_that("feat_max works", {
  res <- feat_max(.data = tsibble::pedestrian, y = "Count")

  expect_equal(nrow(res), 4)
})

test_that("feat_min works", {
  res <- feat_min(.data = tsibble::pedestrian, y = "Count")

  expect_equal(nrow(res), 191)
})

test_that("feat_max doest not works", {
  expect_error(feat_max(.data = tsibble::pedestrian, y = "gaga"))
})

test_that("feat_min doest not works", {
  expect_error(feat_min(.data = tsibble::pedestrian, y = "gaga"))
})

test_that("feat_max doest not works", {
  expect_error(feat_max(.data = mtcars, y = "mpg"))
})

test_that("feat_min doest not works", {
  expect_error(feat_min(.data = mtcars, y = "mpg"))
})

