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
  feat_length_stat(.data, y, b = 5, b_op = "gte", stat = "max") %>%
    dplyr::rename("Dcmax" = "freq")
}

epi_dcmed <- function(.data, y){
  feat_length_stat(.data, y, b = 5, b_op = "gte", stat = "median") %>%
    dplyr::rename("Dcmed" = "freq")
}

epi_ds3 <- function(.data, y){
  feat_length(.data, y, a_op = "gte", a = 3, b_op = "e", b = 0) %>%
    dplyr::rename("Ds3" = "freq")
}

epi_ds6 <- function(.data, y){
  feat_length(.data, y, a_op = "gte", a = 6, b_op = "e", b = 0) %>%
    dplyr::rename("Ds6" = "freq")
}

epi_dsmax <- function(.data, y){
  feat_length_stat(.data, y, b = 0, b_op = "e", stat = "max") %>%
    dplyr::rename("Dsmax" = "freq")
}

epi_dsmed <- function(.data, y){
  feat_length_stat(.data, y, b = 0, b_op = "e", stat = "median") %>%
    dplyr::rename("Dsmed" = "freq")
}

epi_prop <- function(.data, y){
  feat_prop(.data, y, b = 5, b_op = "gte") %>%
    dplyr::rename("Prop" = "prop")
}

epi_st <- function(.data, y){
  feat_st(.data, y) %>%
    dplyr::rename("ST" = "st")
}
