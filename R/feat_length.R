tidy_rle_filter <- function(.data, y, a_op, a, b_op, b){
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

  res1 <- rle(get("value_ref", .data))

  res2 <- tibble::tibble(
    lengths = res1$lengths,
    values = res1$values
  )

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

  nrow(res3)
}

feat_length <- function(.data, y, a_op = "gte", a, b_op = "gte", b){
  checkmate::assert_class(x = .data, classes = "tbl_ts")
  checkmate::assert_choice(x = y, choices = names(.data))
  checkmate::assert_count(x = a)
  checkmate::assert_count(x = b)

  tsibble::as_tibble(.data) %>%
    dplyr::group_by(!!!tsibble::key(.data)) %>%
    dplyr::summarise(freq = tidy_rle_filter(.data = dplyr::pick(dplyr::everything()), y = y, a_op = a_op, a = a, b_op = b_op, b = b)) %>%
    dplyr::ungroup()
}

