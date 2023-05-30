#' Compute epidemiological features from time series
#'
#' Following
#'
#' @param .data .data .data a \code{tsibble} object.
#' @param y y character. Reference variable with numeric values.
#'
#' @return a \code{tibble} object.
#' @export
#'
#' @describeIn epifeatures Compute Xp and Tp
epi_xp_tp <- function(.data, y){
  feat_max(.data, y) %>%
    tibble::as_tibble() %>%
    dplyr::select(
      tsibble::key_vars(.data),
      "Xp" = dplyr::all_of(y),
      "Tp" = tsibble::index_var(.data)
    )
}

#' @describeIn epifeatures Compute Dc3
epi_dc3 <- function(.data, y){
  feat_length(.data, y, a_op = "gte", a = 3, b_op = "gte", b = 5) %>%
    dplyr::rename("Dc3" = "freq")
}

#' @describeIn epifeatures Compute Dc6
epi_dc6 <- function(.data, y){
  feat_length(.data, y, a_op = "gte", a = 6, b_op = "gte", b = 5) %>%
    dplyr::rename("Dc6" = "freq")
}

#' @describeIn epifeatures Compute Dcmax
epi_dcmax <- function(.data, y){
  feat_length_stat(.data, y, b = 5, b_op = "gte", stat = "max") %>%
    dplyr::rename("Dcmax" = "freq")
}

#' @describeIn epifeatures Compute Dcmed
epi_dcmed <- function(.data, y){
  feat_length_stat(.data, y, b = 5, b_op = "gte", stat = "median") %>%
    dplyr::rename("Dcmed" = "freq")
}

#' @describeIn epifeatures Compute Ds3
epi_ds3 <- function(.data, y){
  feat_length(.data, y, a_op = "gte", a = 3, b_op = "e", b = 0) %>%
    dplyr::rename("Ds3" = "freq")
}

#' @describeIn epifeatures Compute Ds6
epi_ds6 <- function(.data, y){
  feat_length(.data, y, a_op = "gte", a = 6, b_op = "e", b = 0) %>%
    dplyr::rename("Ds6" = "freq")
}

#' @describeIn epifeatures Compute Dsmax
epi_dsmax <- function(.data, y){
  feat_length_stat(.data, y, b = 0, b_op = "e", stat = "max") %>%
    dplyr::rename("Dsmax" = "freq")
}

#' @describeIn epifeatures Compute Dsmed
epi_dsmed <- function(.data, y){
  feat_length_stat(.data, y, b = 0, b_op = "e", stat = "median") %>%
    dplyr::rename("Dsmed" = "freq")
}

#' @describeIn epifeatures Compute P+ (Prop)
epi_prop <- function(.data, y){
  feat_prop(.data, y, b = 5, b_op = "gte") %>%
    dplyr::rename("Prop" = "prop")
}

#' @describeIn epifeatures Compute ST
epi_st <- function(.data, y){
  feat_st(.data, y, type = "mult") %>%
    dplyr::rename("ST" = "st")
}
