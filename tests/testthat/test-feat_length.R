example_1 <- tsibble::tsibble(
  cod = rep(1, 10),
  time = 1:10,
  value = c(8,15,20,0,0,0,0,5,9,12),key = cod, index = time
)

example_2 <- tsibble::tsibble(
  cod = rep(1, 10),
  time = 1:10,
  value = c(5,8,9,10,8,6,0,0,0,12),key = cod, index = time
)

example_3 <- tsibble::tsibble(
  cod = c(rep(1, 10),rep(2, 10)),
  time = c(1:10,1:10),
  value = c(c(8,15,20,0,0,0,0,5,9,12),c(8,15,20,0,0,0,0,5,9,12)),
  key = cod, index = time
)

# example_4 <- tsibble::tsibble(
#   cod = rep(1, 11),
#   time = 1:11,
#   value = c(0,16,0,0,8,3,0,10,0,2,0),key = cod, index = time
# )

example_5 <- tsibble::tsibble(
  cod = rep(1, 11),
  time = 1:11,
  value = c(2,0,0,0,3,5,0,0,0,0,2),key = cod, index = time
)

test_that("feat_length works with at least 3 consecutive periods with value equal or greater to 5", {
  res <- feat_length(.data = example_1, y = "value", a_op = "gte", a = 3, b_op = "gte", b = 5)

  expect_equal(nrow(res), 1)
  expect_equal(res$freq, 2)
})

test_that("feat_length works with example at least 6 consecutive periods with value equal or greater to 5", {
  res <- feat_length(.data = example_2, y = "value", a_op = "gte", a = 6, b_op = "gte", b = 5)

  expect_equal(nrow(res), 1)
  expect_equal(res$freq, 1)
})

test_that("feat_length works with at least 3 consecutive periods with value equal or greater to 5", {
  res <- feat_length(.data = example_3, y = "value", a_op = "gte", a = 3, b_op = "gte", b = 5)

  expect_equal(nrow(res), 2)
  expect_equal(res$freq[[1]], 2)
  expect_equal(res$freq[[2]], 2)
})

# test_that("feat_length works with isolate periods with value equal or greater to 5", {
#   res1 <- feat_length(.data = example_4, y = "value", a_op = "e", a = 0, b_op = "gte", b = 5)
#
#   expect_equal(nrow(res1), 1)
#   expect_equal(res1$freq, 2)
# })

test_that("feat_length works with at least three consecutive periods with no counts", {
  res <- feat_length(.data = example_5, y = "value", a_op = "gte", a = 3, b_op = "e", b = 0)

  expect_equal(nrow(res), 1)
  expect_equal(res$freq, 2)
})
