test_that("feat_max works", {
  res <- feat_max(x = tsibble::pedestrian, y = "Count")

  expect_equal(nrow(res), 4)
})

test_that("feat_min works", {
  res <- feat_min(x = tsibble::pedestrian, y = "Count")

  expect_equal(nrow(res), 191)
})

test_that("feat_max doest not works", {
  expect_error(feat_max(x = tsibble::pedestrian, y = "gaga"))
})

test_that("feat_min doest not works", {
  expect_error(feat_min(x = tsibble::pedestrian, y = "gaga"))
})

test_that("feat_max doest not works", {
  expect_error(feat_max(x = mtcars, y = "mpg"))
})

test_that("feat_min doest not works", {
  expect_error(feat_min(x = mtcars, y = "mpg"))
})

