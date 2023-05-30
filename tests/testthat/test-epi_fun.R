# Tests with paper examples

test_that("epi_xp_tp works", {
  example_data <- tsibble::tsibble(
    cod = rep(1, 10),
    time = 1:10,
    count = c(8,15,20,0,0,0,0,5,9,12),
    key = cod,
    index = time
  )

  res <- epi_xp_tp(example_data, "count")

  expect_equal(res$Xp, 20)
  expect_equal(res$Tp, 3)
})

test_that("epi_dc3 works", {
  example_data <- tsibble::tsibble(
    cod = rep(1, 10),
    time = 1:10,
    count = c(8,15,20,0,0,0,0,5,9,12),
    key = cod,
    index = time
  )

  res <- epi_dc3(example_data, "count")

  expect_equal(res$Dc3, 2)
})

test_that("epi_dc6 works", {
  example_data <- tsibble::tsibble(
    cod = rep(1, 10),
    time = 1:10,
    count = c(5,8,9,10,8,6,0,0,0,12),
    key = cod,
    index = time
  )

  res <- epi_dc6(example_data, "count")

  expect_equal(res$Dc6, 1)
})

test_that("epi_dcmax works", {
  example_data <- tsibble::tsibble(
    cod = rep(1, 10),
    time = 1:10,
    count = c(5,8,9,10,8,6,0,0,0,12),
    key = cod,
    index = time
  )

  res <- epi_dcmax(example_data, "count")

  expect_equal(res$Dcmax, 6)
})

test_that("epi_dcmed works", {
  example_data <- tsibble::tsibble(
    cod = rep(1, 10),
    time = 1:10,
    count = c(5,8,9,10,8,6,0,0,0,12),
    key = cod,
    index = time
  )

  res <- epi_dcmed(example_data, "count")

  expect_equal(res$Dcmed, 3.5)
})

test_that("epi_ds3 works", {
  example_data <- tsibble::tsibble(
    cod = rep(1, 11),
    time = 1:11,
    count = c(2,0,0,0,3,5,0,0,0,0,2),
    key = cod,
    index = time
  )

  res <- epi_ds3(example_data, "count")

  expect_equal(res$Ds3, 2)
})

test_that("epi_ds6 works", {
  example_data <- tsibble::tsibble(
    cod = rep(1, 13),
    time = 1:13,
    count = c(3,0,0,0,0,0,0,3,5,0,0,0,2),
    key = cod,
    index = time
  )

  res <- epi_ds6(example_data, "count")

  expect_equal(res$Ds6, 1)
})

test_that("epi_dsmax works", {
  example_data <- tsibble::tsibble(
    cod = rep(1, 10),
    time = 1:10,
    count = c(5,8,9,10,8,6,0,0,0,12),
    key = cod,
    index = time
  )

  res <- epi_dsmax(example_data, "count")

  expect_equal(res$Dsmax, 3)
})

test_that("epi_dsmed works", {
  example_data <- tsibble::tsibble(
    cod = rep(1, 18),
    time = 1:18,
    count = c(5,8,9,10,8,6,0,0,0,12,0,0,0,0,3,0,0,0),
    key = cod,
    index = time
  )

  res <- epi_dsmed(example_data, "count")

  expect_equal(res$Dsmed, 3)
})

test_that("epi_prop works", {
  example_data <- tsibble::tsibble(
    cod = rep(1, 10),
    time = 1:10,
    count = c(5,8,9,10,8,6,0,0,0,12),
    key = cod,
    index = time
  )

  res <- epi_prop(example_data, "count")

  expect_equal(res$Prop, 0.7)
})

test_that("epi_st works", {
  res <- epi_st(tsibble::as_tsibble(USAccDeaths), "value")

  expect_equal(res$ST, 8682.6993)
})




# Tests with big dataset

example_data_big <- outbreaks::covid19_england_nhscalls_2020 %>%
  dplyr::group_by(date, postcode) %>%
  dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  dplyr::ungroup() %>%
  tsibble::as_tsibble(key = postcode, index = date)

test_that("epi_xp_tp works with big dataset", {
  res <- epi_xp_tp(example_data_big, "count")

  expect_equal(rlang::hash(res$Xp), "59eca778e4f83fb1ff33ba052e5e9ba2")
  expect_equal(rlang::hash(res$Tp), "fa73650ecbbba1350792a8178763b92a")
})

test_that("epi_dc3 works with big dataset", {
  res <- epi_dc3(example_data_big, "count")

  expect_equal(rlang::hash(res$Dc3), "ceb8324a497d774f5646299237776536")
})

test_that("epi_dc6 works with big dataset", {
  res <- epi_dc6(example_data_big, "count")

  expect_equal(rlang::hash(res$Dc6), "c6b7ab068b61bd1e60ebd964947d98a8")
})

test_that("epi_dcmax works with big dataset", {
  res <- epi_dcmax(example_data_big, "count")

  expect_equal(rlang::hash(res$Dcmax), "b55de930f39dd2160d8323710408cc98")
})

test_that("epi_dcmed works with big dataset", {
  res <- epi_dcmed(example_data_big, "count")

  expect_equal(rlang::hash(res$Dcmed), "3caae7ed1c49793c093d0b2601194659")
})

test_that("epi_ds3 works with big dataset", {
  res <- epi_ds3(example_data_big, "count")

  expect_equal(rlang::hash(res$Ds3), "4f70a9347e5ff6e489f57c46483caf65")
})

test_that("epi_ds6 works with big dataset", {
  res <- epi_ds6(example_data_big, "count")

  expect_equal(rlang::hash(res$Ds6), "4f70a9347e5ff6e489f57c46483caf65")
})

test_that("epi_dsmax works with big dataset", {
  res <- epi_dsmax(example_data_big, "count")

  expect_equal(rlang::hash(res$Dsmax), "cd4de9ef54b037deb7343ccdfa4b77dc")
})

test_that("epi_dsmed works with big dataset", {
  res <- epi_dsmed(example_data_big, "count")

  expect_equal(rlang::hash(res$Dsmed), "cd4de9ef54b037deb7343ccdfa4b77dc")
})

test_that("epi_prop works with big dataset", {
  res <- epi_prop(example_data_big, "count")

  expect_equal(rlang::hash(res$Prop), "335bac5b5ead5736781cafd04d73e50b")
})

test_that("epi_st works with big dataset", {
  res <- epi_st(example_data_big, "count")

  expect_equal(rlang::hash(res$ST), "d9721fde2cd87f62c38a3bd356d0fde0")
})
