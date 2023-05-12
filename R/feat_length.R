tidy_rle <- function(x, y, a, b){
  checkmate::assert_class(x = x, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(x))
  checkmate::assert_count(x = a)
  checkmate::assert_count(x = b)

  x$value_ref <- ifelse(get(y, x) >= b, TRUE, FALSE)

  res1 <- rle(get("value_ref", x))

  res2 <- tibble::tibble(
    lengths = res1$lengths,
    values = res1$values
  )

  dplyr::filter(.data = res2, values == TRUE & lengths >= a) %>%
    nrow()
}

feat_length <- function(x, y, a, b){
  checkmate::assert_class(x = x, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(x))
  checkmate::assert_count(x = a)
  checkmate::assert_count(x = b)

  #tsibble::group_by_key(.data = x, .drop = TRUE) %>%
  tibble::as_tibble(x) %>%
  dplyr::group_by_at(tsibble::key_vars(x)) %>%
    dplyr::summarise(freq = tidy_rle(x, y, a, b))
}
