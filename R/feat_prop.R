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

  return(sum(.data$value_ref)/nrow(.data))
}
