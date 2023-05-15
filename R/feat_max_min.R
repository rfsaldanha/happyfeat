feat_max <- function(.data, y){
  checkmate::assert_class(x = .data, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(.data))

  tsibble::group_by_key(.data = .data) %>%
    dplyr::filter(get(y) == max(get(y))) %>%
    dplyr::ungroup()
}

feat_min <- function(.data, y){
  checkmate::assert_class(x = .data, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(.data))

  tsibble::group_by_key(.data = .data) %>%
    dplyr::filter(get(y) == min(get(y))) %>%
    dplyr::ungroup()
}
