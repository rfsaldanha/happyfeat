rle_filter <- function(.data, y, a_op, a, b_op, b, isolated){
  checkmate::assert_class(x = .data, classes = "tbl")
  checkmate::assert_choice(x = y, choices = names(.data))
  checkmate::assert_count(x = a)
  checkmate::assert_count(x = b)

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

  if(isolated == TRUE){
    .data$lag <- dplyr::lag(get(y, .data), default = 0)
    .data$lead <- dplyr::lead(get(y, .data), default = 0)
    .data$value_ref_2 <- ifelse(.data$lag == 0 & .data$lead == 0, TRUE, FALSE)
    .data$value_ref <- as.logical(.data$value_ref * .data$value_ref_2)
  }

  res1 <- rle(get("value_ref", .data))

  res2 <- tibble::tibble(
    lengths = res1$lengths,
    values = res1$values
  )


  if(isolated == FALSE){
    if(a_op == "gte"){
      res3 <- dplyr::filter(.data = res2, .data$values == TRUE & .data$lengths >= a)
    } else if(a_op == "lte"){
      res3 <- dplyr::filter(.data = res2, .data$values == TRUE & .data$lengths <= a)
    } else if(a_op == "gt"){
      res3 <- dplyr::filter(.data = res2, .data$values == TRUE & .data$lengths > a)
    } else if(a_op == "lt"){
      res3 <- dplyr::filter(.data = res2, .data$values == TRUE & .data$lengths < a)
    } else if(a_op == "e"){
      res3 <- dplyr::filter(.data = res2, .data$values == TRUE & .data$lengths == a)
    }
  } else if(isolated == TRUE){

    if(a != 1L){
      stop("Argument `a` must be equal to 1.")
    }

    if(a_op == "gte"){
      res3 <- dplyr::filter(.data = res2, .data$values == TRUE & .data$lengths == a)
    } else if(a_op == "lte"){
      res3 <- dplyr::filter(.data = res2, .data$values == TRUE & .data$lengths == a)
    } else if(a_op == "gt"){
      res3 <- dplyr::filter(.data = res2, .data$values == TRUE & .data$lengths == a)
    } else if(a_op == "lt"){
      res3 <- dplyr::filter(.data = res2, .data$values == TRUE & .data$lengths == a)
    } else if(a_op == "e"){
      res3 <- dplyr::filter(.data = res2, .data$values == TRUE & .data$lengths == a)
    }
  }


  nrow(res3)
}

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
  checkmate::assert_class(x = .data, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(.data))
  checkmate::assert_choice(x = a_op, choices = c("gte", "lte", "gt", "lt", "e"))
  checkmate::assert_choice(x = b_op, choices = c("gte", "lte", "gt", "lt", "e"))
  checkmate::assert_count(x = a)
  checkmate::assert_count(x = b)

  tsibble::as_tibble(.data) %>%
    dplyr::group_by(!!!tsibble::key(.data)) %>%
    dplyr::summarise(freq = rle_filter(.data = dplyr::pick(dplyr::everything()), y = y, a_op = a_op, a = a, b_op = b_op, b = b, isolated = isolated)) %>%
    dplyr::ungroup()
}

