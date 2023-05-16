example_data <- outbreaks::covid19_england_nhscalls_2020 %>%
  dplyr::group_by(date, postcode) %>%
  dplyr::summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
  dplyr::ungroup() %>%
  tsibble::as_tsibble(key = postcode, index = date)

test_that("epi_xp_tp works", {
  res <- epi_xp_tp(example_data, "count")

  expect_equal(rlang::hash(res$Xp), "59eca778e4f83fb1ff33ba052e5e9ba2")
  expect_equal(rlang::hash(res$Tp), "fa73650ecbbba1350792a8178763b92a")
})

test_that("epi_dc3 works", {
  res <- epi_dc3(example_data, "count")

  expect_equal(rlang::hash(res$Dc3), "ceb8324a497d774f5646299237776536")
})

test_that("epi_dc6 works", {
  res <- epi_dc6(example_data, "count")

  expect_equal(rlang::hash(res$Dc6), "c6b7ab068b61bd1e60ebd964947d98a8")
})

test_that("epi_dcmax works", {
  res <- epi_dcmax(example_data, "count")

  expect_equal(rlang::hash(res$Dcmax), "b55de930f39dd2160d8323710408cc98")
})
