feat_st <- function(.data, y, type){
  # Check assertions
  checkmate::assert_class(x = .data, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(.data))
  checkmate::assert_choice(x = type, choices = c("mult", "add"))

  index_var <- tsibble::index_var(.data)

  tsibble::as_tibble(.data) %>%
    dplyr::group_by(!!!tsibble::key(.data)) %>%
    dplyr::summarise(st = st_ratio(dplyr::pick(dplyr::everything()), y = y, index_var = index_var, type = type)) %>%
    dplyr::ungroup()
}


st_ratio <- function(.data, y, index_var, type){
  # Check assertions
  checkmate::assert_class(x = .data, classes = "tbl")
  checkmate::assert_choice(x = y, choices = names(.data))

  res <- tsibble::as_tsibble(.data, index = index_var)

  if(tsibble::has_gaps(res)$.gaps){
    return(NA)
  } else if(stats::var(get(y, res)) == 0){
    return(NA)
  } else {
    dec_components <- fabletools::model(res, feasts::classical_decomposition(!!rlang::sym(y), type = type)) %>%
      fabletools::components()

    ratio <- mean(dec_components$trend, na.rm = TRUE)/mean(dec_components$seasonal, na.rm = TRUE)

    return(ratio)
  }
}
