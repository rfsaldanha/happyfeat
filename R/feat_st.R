feat_st <- function(.data, y){
  # Check assertions
  checkmate::assert_class(x = .data, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(.data))

  index_var <- tsibble::index_var(.data)

  tsibble::as_tibble(.data) %>%
    dplyr::group_by(!!!tsibble::key(.data)) %>%
    dplyr::summarise(st = st_ratio(dplyr::pick(dplyr::everything()), y = y, index_var = index_var)) %>%
    dplyr::ungroup()
}


st_ratio <- function(.data, y, index_var){
  # Check assertions
  checkmate::assert_class(x = .data, classes = "tbl")
  checkmate::assert_choice(x = y, choices = names(.data))

  res <- tsibble::as_tsibble(.data, index = index_var)

  if(tsibble::has_gaps(res)$.gaps){
    return(NA)
  } else if(var(get(y, res)) == 0){
    return(NA)
  } else {
    dec_components <- fabletools::model(res, feasts::classical_decomposition(!!rlang::sym(y), type = "mult")) %>%
      fabletools::components()

    ratio <- mean(dec_components$trend, na.rm = TRUE)/mean(dec_components$seasonal, na.rm = TRUE)

    return(ratio)
  }
}
