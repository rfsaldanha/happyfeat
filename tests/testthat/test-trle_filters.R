test_that("trle_filter works", {
  example_1 <- tsibble::tsibble(
    cod = rep(1, 10),
    time = 1:10,
    value = c(8,15,20,0,0,0,0,5,9,12),
    key = cod,
    index = time
  )

  expect_equal(trle_filter(.data = example_1, y = "value", a_op = "gte", a = 3, b_op = "gte", b = 5, isolated = FALSE), 2)
})

test_that("trle_filter_max works", {
  example_7 <- tibble::tibble(
    cod = rep(1, 13),
    time = 1:13,
    value = c(8,15,20,0,0,0,0,5,0,0,0,NA,12)
  )

  expect_equal(trle_filter_max(.data = example_7, y = "value", b = 0, b_op = "e"), 4)
})
