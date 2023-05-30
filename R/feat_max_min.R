#' Isolate rows with observed maximum or minimum value
#'
#' This function will filter -- for each key in a tsibble object -- the rows that contain maximum or minimum value.
#'
#' @param .data a \code{tsibble} object.
#' @param y character. Reference variable with numeric values.
#'
#' @return a \code{tsibble} object.
#' @export
#'
#' @examples
#' example_data <- tsibble::tsibble(
#' cod = rep(1, 10),
#' time = 1:10,
#' value = c(8,15,20,0,0,0,0,5,9,12),
#' key = cod,
#' index = time
#' )
#'
#' feat_max(.data = example_data, y = "value")
#'
feat_max <- function(.data, y){
  checkmate::assert_class(x = .data, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(.data))

  tsibble::group_by_key(.data = .data) %>%
    dplyr::filter(get(y) == max(get(y))) %>%
    dplyr::ungroup()
}

#' @rdname feat_max
feat_min <- function(.data, y){
  checkmate::assert_class(x = .data, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(.data))

  tsibble::group_by_key(.data = .data) %>%
    dplyr::filter(get(y) == min(get(y))) %>%
    dplyr::ungroup()
}
