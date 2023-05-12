feat_max <- function(x, y){
  checkmate::assert_class(x = x, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(x))

  x %>% tsibble::group_by_key() %>%
    dplyr::filter(get(y) == max(get(y)))
}

feat_min <- function(x, y){
  checkmate::assert_class(x = x, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(x))

  x %>% tsibble::group_by_key() %>%
    dplyr::filter(get(y) == min(get(y)))
}
