#' Compute the proportion of values that meets criteria
#'
#' This function computes -- for each key on a tsibble object -- the proportion (from 0 to 1) of values that meets criteria.
#'
#' Example: What is the proportion of values greater or equal to 5 (\code{b = 5, b_op = "gte"})?
#'
#' @param .data a \code{tsibble} object.
#' @param y character. Reference variable with numeric values.
#' @param b integer. Value threshold applied to \code{y}.
#' @param b_op character. Operator, \code{gte} = greater than or equal, \code{lte} = less than or equal, \code{gt} = greater than, \code{lt} = less than, \code{e} = equal.
#'
#' @return a \code{tibble} object.
#' @export
#'
#' @examples
#'
#' # What is the proportion of values greater or equal to 5?
#'
#' example_data <- tsibble::tsibble(
#' cod = rep(1, 10),
#' time = 1:10,
#' value = c(8,15,20,0,0,0,0,5,9,12),
#' key = cod,
#' index = time
#' )
#'
#' feat_prop(.data = example_data, y = "value", b = 5, b_op = "gte")
#'
#'
feat_prop <- function(.data, y, b, b_op){
  # Check assertions
  checkmate::assert_class(x = .data, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(.data))
  checkmate::assert_count(x = b)
  checkmate::assert_choice(x = b_op, choices = c("gte", "lte", "gt", "lt", "e"))

  # Apply trle_filter_max for each keys on a tsibble object
  tsibble::as_tibble(.data) %>%
    dplyr::group_by(!!!tsibble::key(.data)) %>%
    dplyr::summarise(prop = t_filter(.data = dplyr::pick(dplyr::everything()), y = y, b = b, b_op = b_op)) %>%
    dplyr::ungroup()
}

t_filter <- function(.data, y, b, b_op){
  # Check assertions
  checkmate::assert_class(x = .data, classes = "tbl")
  checkmate::assert_choice(x = y, choices = names(.data))
  checkmate::assert_count(x = b)
  checkmate::assert_choice(x = b_op, choices = c("gte", "lte", "gt", "lt", "e"))

  # Create a logical variable, operating y and b
  if(b_op == "gte"){
    .data$value_ref <- ifelse(get(y, .data) >= b, TRUE, FALSE)
  } else if(b_op == "lte"){
    .data$value_ref <- ifelse(get(y, .data) <= b, TRUE, FALSE)
  } else if(b_op == "gt"){
    .data$value_ref <- ifelse(get(y, .data) > b, TRUE, FALSE)
  } else if(b_op == "lt"){
    .data$value_ref <- ifelse(get(y, .data) < b, TRUE, FALSE)
  } else if(b_op == "e"){
    .data$value_ref <- ifelse(get(y, .data) == b, TRUE, FALSE)
  }

  return(sum(.data$value_ref, na.rm = TRUE)/nrow(.data))
}
