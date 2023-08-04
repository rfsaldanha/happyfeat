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

test_data <- tsibble::pedestrian %>%
  dplyr::group_by(Sensor) %>%
  dplyr::mutate(sensor2 = sample(x = 1:10, size = dplyr::n(), replace = TRUE)) %>%
  dplyr::ungroup() %>%
  tsibble::as_tsibble(key = c(sensor2, Sensor))

test_that("feat_max works with two keys", {
  res <- feat_max(.data = test_data, y = "Count")

  expect_equal(nrow(res), 40)
})
