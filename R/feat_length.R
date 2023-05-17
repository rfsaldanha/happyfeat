#' Compute length of periods
#'
#' @param .data a \code{tsibble} object.
#' @param y character. Reference variable with numeric values.
#' @param a_op,b_op character. Operator, \code{gte} = greater than or equal, \code{lte} = less than or equal, \code{gt} = greater than, \code{lt} = less than, \code{e} = equal.
#' @param a integer. Length of period threshold.
#' @param b integer. Value threshold applied to \code{y}.
#' @param isolated logical. Consider only isolated events, ie. surrounded by zeros. On this case, \code{a} and \code{a_op} are not considered.
#'
#' @return A tibble.
#' @export
#'
#' @examples
#' # How many at least 3 consecutive periods with value equal or greater to 5
#'
#' example_1 <- tsibble::tsibble(
#' cod = rep(1, 10),
#' time = 1:10,
#' value = c(8,15,20,0,0,0,0,5,9,12),
#' key = cod,
#' index = time
#' )
#'
#' feat_length(.data = example_1, y = "value", a_op = "gte", a = 3, b_op = "gte", b = 5)
#'
#' # How many isolate periods with value equal or greater to 5
#'
#' example_4 <- tsibble::tsibble(
#' cod = rep(1, 11),
#' time = 1:11,
#' value = c(0,16,0,0,8,3,0,10,0,2,0),key = cod, index = time
#' )
#'
#' feat_length(.data = example_4, y = "value", a_op = "e", a = 1, b_op = "gte", b = 5, isolated = TRUE)
#'
feat_length <- function(.data, y, a_op = "gte", a, b_op = "gte", b, isolated = FALSE){
  # Check assertions
  checkmate::assert_class(x = .data, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(.data))
  checkmate::assert_choice(x = a_op, choices = c("gte", "lte", "gt", "lt", "e"))
  checkmate::assert_choice(x = b_op, choices = c("gte", "lte", "gt", "lt", "e"))
  checkmate::assert_count(x = a)
  checkmate::assert_count(x = b)

  # Apply trle_filter for each keys on a tsibble object
  tsibble::as_tibble(.data) %>%
    dplyr::group_by(!!!tsibble::key(.data)) %>%
    dplyr::summarise(freq = trle_filter(.data = dplyr::pick(dplyr::everything()), y = y, a_op = a_op, a = a, b_op = b_op, b = b, isolated = isolated)) %>%
    dplyr::ungroup()
}




feat_length_stat <- function(.data, y, b, b_op = "e", stat){
  # Check assertions
  checkmate::assert_class(x = .data, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(.data))
  checkmate::assert_count(x = b)
  checkmate::assert_choice(x = b_op, choices = c("gte", "lte", "gt", "lt", "e"))

  # Apply trle_filter_max for each keys on a tsibble object
  tsibble::as_tibble(.data) %>%
    dplyr::group_by(!!!tsibble::key(.data)) %>%
    dplyr::summarise(freq = trle_filter_stat(.data = dplyr::pick(dplyr::everything()), y = y, b = b, b_op = b_op, stat = stat)) %>%
    dplyr::ungroup()
}
