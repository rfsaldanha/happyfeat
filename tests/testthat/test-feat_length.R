test_that("feat_length works with at least 3 consecutive periods with value equal or greater to 5", {
  example_1 <- tsibble::tsibble(
    cod = rep(1, 10),
    time = 1:10,
    value = c(8,15,20,0,0,0,0,5,9,12),
    key = cod,
    index = time
  )

  res <- feat_length(.data = example_1, y = "value", a_op = "gte", a = 3, b_op = "gte", b = 5)

  expect_equal(nrow(res), 1)
  expect_equal(res$freq, 2)
})

test_that("feat_length works with example at least 6 consecutive periods with value equal or greater to 5", {
  example_2 <- tsibble::tsibble(
    cod = rep(1, 10),
    time = 1:10,
    value = c(5,8,9,10,8,6,0,0,0,12),key = cod, index = time
  )

  res <- feat_length(.data = example_2, y = "value", a_op = "gte", a = 6, b_op = "gte", b = 5)

  expect_equal(nrow(res), 1)
  expect_equal(res$freq, 1)
})

test_that("feat_length works with at least 3 consecutive periods with value equal or greater to 5", {
  example_3 <- tsibble::tsibble(
    cod = c(rep(1, 10),rep(2, 10)),
    time = c(1:10,1:10),
    value = c(c(8,15,20,0,0,0,0,5,9,12),c(8,15,20,0,0,0,0,5,9,12)),
    key = cod, index = time
  )

  res <- feat_length(.data = example_3, y = "value", a_op = "gte", a = 3, b_op = "gte", b = 5)

  expect_equal(nrow(res), 2)
  expect_equal(res$freq[[1]], 2)
  expect_equal(res$freq[[2]], 2)
})

test_that("feat_length works with isolate periods with value equal or greater to 5", {
  example_4 <- tsibble::tsibble(
    cod = rep(1, 11),
    time = 1:11,
    value = c(0,16,0,0,8,3,0,10,0,2,0),key = cod, index = time
  )

  res1 <- feat_length(.data = example_4, y = "value", a_op = "e", a = 1, b_op = "gte", b = 5, isolated = TRUE)

  expect_equal(nrow(res1), 1)
  expect_equal(res1$freq, 2)
})

test_that("feat_length does not works with isolate and a different of 1 and a operator different of e", {
  example_4 <- tsibble::tsibble(
    cod = rep(1, 11),
    time = 1:11,
    value = c(0,16,0,0,8,3,0,10,0,2,0),key = cod, index = time
  )

  expect_error(feat_length(.data = example_4, y = "value", a_op = "e", a = 2, b_op = "gte", b = 5, isolated = TRUE))
  expect_error(feat_length(.data = example_4, y = "value", a_op = "gt", a = 1, b_op = "gte", b = 5, isolated = TRUE))
  expect_error(feat_length(.data = example_4, y = "value", a_op = "lte", a = 2, b_op = "gte", b = 5, isolated = TRUE))
})

test_that("feat_length works with at least three consecutive periods with no counts", {
  example_5 <- tsibble::tsibble(
    cod = rep(1, 11),
    time = 1:11,
    value = c(2,0,0,0,3,5,0,0,0,0,2),key = cod, index = time
  )

  res <- feat_length(.data = example_5, y = "value", a_op = "gte", a = 3, b_op = "e", b = 0)

  expect_equal(nrow(res), 1)
  expect_equal(res$freq, 2)
})

test_that("feat_length works with at least six consecutive periods with no counts", {
  example_5 <- tsibble::tsibble(
    cod = rep(1, 13),
    time = 1:13,
    value = c(3,0,0,0,0,0,0,3,5,0,0,0,2),key = cod, index = time
  )

  res <- feat_length(.data = example_5, y = "value", a_op = "gte", a = 6, b_op = "e", b = 0)

  expect_equal(nrow(res), 1)
  expect_equal(res$freq, 1)
})

test_that("feat_length works with NA", {
  example_6 <- tsibble::tsibble(
    cod = rep(1, 10),
    time = 1:10,
    value = c(8,15,20,0,0,0,0,5,NA,12),key = cod, index = time
  )

  res <- feat_length(.data = example_6, y = "value", a_op = "gte", a = 3, b_op = "gte", b = 5)

  expect_equal(nrow(res), 1)
  expect_equal(res$freq, 1)
})



test_that("feat_length_max works", {
  example_8 <- tsibble::tsibble(
    cod = rep(1, 13),
    time = 1:13,
    value = c(8,15,20,0,0,0,0,5,0,0,0,NA,12),key = cod, index = time
  )

  res <- feat_length_max(.data = example_8, y = "value", b = 0)

  expect_equal(nrow(res), 1)
  expect_equal(res$freq, 4)
})

test_that("feat_length_max works with more keys", {
  example_9 <- tsibble::tsibble(
    cod = c(rep(1, 10),rep(2, 10)),
    time = c(1:10,1:10),
    value = c(c(8,15,20,0,0,0,0,0,9,12),c(8,15,20,0,0,0,0,5,9,12)),
    key = cod, index = time
  )

  res <- feat_length_max(.data = example_9, y = "value", b = 0)

  expect_equal(nrow(res), 2)
  expect_equal(res$freq[[1]], 5)
  expect_equal(res$freq[[2]], 4)
})
