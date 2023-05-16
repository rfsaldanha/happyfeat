test_that("trle works", {
  example_1 <- tibble::tibble(
    cod = rep(1, 10),
    time = 1:10,
    value = c(8,15,20,0,0,0,0,5,9,12)
  )

  res1 <- trle(.data = example_1, y = "value")
  res2 <- rle(example_1$value)

  expect_equal(res1$lengths, res2[[1]])
  expect_equal(res1$values, res2[[2]])
})
