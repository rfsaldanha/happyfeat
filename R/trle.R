#' Run Length Encoding and return result as a tibble
#'
#' @param .data a \code{tibble} object.
#' @param y character. A variable available on \code{.data}
#'
#' @return a \code{tibble} object.
#' @export
#'
#' @examples
#' example_1 <- tibble::tibble(
#' cod = rep(1, 10),
#' time = 1:10,
#' value = c(8,15,20,0,0,0,0,5,9,12)
#' )
#'
#' trle(.data = example_1, y = "value")
trle <- function(.data, y){
  # Check assertions
  checkmate::assert_class(x = .data, classes = "tbl")
  checkmate::assert_choice(x = y, choices = names(.data))

  # Run length encoding
  res <- rle(get(y, .data))

  # Tibble result
  tibble::tibble(
    lengths = res$lengths,
    values = res$values
  )
}
