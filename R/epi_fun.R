epi_xp_tp <- function(.data, y){
  feat_max(.data, y) %>%
    tibble::as_tibble() %>%
    dplyr::select(
      tsibble::key_vars(.data),
      "Xp" = dplyr::all_of(y),
      "Tp" = tsibble::index_var(.data)
    )
}

epi_dc3 <- function(.data, y){
  feat_length(.data, y, a_op = "gte", a = 3, b_op = "gte", b = 5) %>%
    dplyr::rename("Dc3" = "freq")
}

epi_dc6 <- function(.data, y){
  feat_length(.data, y, a_op = "gte", a = 6, b_op = "gte", b = 5) %>%
    dplyr::rename("Dc6" = "freq")
}

epi_dcmax <- function(.data, y){
  feat_length_max(.data, y, b = 5, b_op = "gte") %>%
    dplyr::rename("Dcmax" = "freq")
}
