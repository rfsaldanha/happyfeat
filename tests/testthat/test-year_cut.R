example_data <- tsibble::tsibble(
  cod = rep(1, 3653),
  time = seq.Date(as.Date("2010-01-01"), as.Date("2020-01-01"), by = "day"),
  count = runif(3653),
  key = cod,
  index = time
)

test_that("year_cut works with epiweek", {
  res <- example_data %>%
    year_cut(cut = 35, cut_type = "epiweek")

  expect_equal(rlang::hash(res$year_cut), "9f19b212872edd3758c2179b235fc8b3")
})

test_that("year_cut works with month", {
  res <- example_data %>%
    year_cut(cut = 4, cut_type = "month")

  expect_equal(rlang::hash(res$year_cut), "f6579a41bcb83672f58d1a792ae645ab")
})


example_data_2 <- tsibble::tsibble(
  cod = rep(1, 3653),
  cod2 = c(rep(1, 3000), rep(2, 653)),
  time = seq.Date(as.Date("2010-01-01"), as.Date("2020-01-01"), by = "day"),
  count = runif(3653),
  key = c(cod, cod2),
  index = time
)

test_that("year_cut works with 2 keys", {
  res <- example_data_2 %>%
    year_cut(cut = 35, cut_type = "epiweek")

  expect_equal(rlang::hash(res$year_cut), "9f19b212872edd3758c2179b235fc8b3")
})
