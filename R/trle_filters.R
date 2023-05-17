trle_filter <- function(.data, y, a_op, a, b_op, b, isolated){
  # Check assertions
  checkmate::assert_class(x = .data, classes = "tbl")
  checkmate::assert_choice(x = y, choices = names(.data))
  checkmate::assert_choice(x = a_op, choices = c("gte", "lte", "gt", "lt", "e"))
  checkmate::assert_choice(x = b_op, choices = c("gte", "lte", "gt", "lt", "e"))
  checkmate::assert_count(x = a)
  checkmate::assert_count(x = b)

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

  # For isolated true, consider if previous and ahead values are equal to zero
  if(isolated == TRUE){
    .data$lag <- dplyr::lag(get(y, .data), default = 0)
    .data$lead <- dplyr::lead(get(y, .data), default = 0)
    .data$value_ref_2 <- ifelse(.data$lag == 0 & .data$lead == 0, TRUE, FALSE)
    .data$value_ref <- as.logical(.data$value_ref * .data$value_ref_2)
  }

  # Tidy length encoding
  res1 <- trle(.data, "value_ref")

  # For isolated false, filter positive results, and operate length and a value
  if(isolated == FALSE){
    if(a_op == "gte"){
      res2 <- dplyr::filter(.data = res1, .data$values == TRUE & .data$lengths >= a)
    } else if(a_op == "lte"){
      res2 <- dplyr::filter(.data = res1, .data$values == TRUE & .data$lengths <= a)
    } else if(a_op == "gt"){
      res2 <- dplyr::filter(.data = res1, .data$values == TRUE & .data$lengths > a)
    } else if(a_op == "lt"){
      res2 <- dplyr::filter(.data = res1, .data$values == TRUE & .data$lengths < a)
    } else if(a_op == "e"){
      res2 <- dplyr::filter(.data = res1, .data$values == TRUE & .data$lengths == a)
    }
  } else if(isolated == TRUE){
    # For isolated true, consider only positive results with length of one
    if(a != 1L){
      stop("Argument `a` must be equal to 1.")
    }

    if(a_op != "e"){
      stop("Argument `a_op` must be equal to `e`.")
    }

    res2 <- dplyr::filter(.data = res1, .data$values == TRUE & .data$lengths == a)
  }

  # Returns the number of rows that meets the specified criteria
  nrow(res2)
}



trle_filter_stat <- function(.data, y, b, b_op, stat){
  # Check assertions
  checkmate::assert_class(x = .data, classes = "tbl")
  checkmate::assert_choice(x = y, choices = names(.data))
  checkmate::assert_count(x = b)
  checkmate::assert_choice(x = b_op, choices = c("gte", "lte", "gt", "lt", "e"))
  checkmate::assert_choice(x = stat, choices = c("max", "min", "mean", "median", "sd", "var"))

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

  # Compute tibble run length encoding considering the reference value, filter positive values, pull lengths and determine maximun value
  res <- trle(.data, y = "value_ref") %>%
    dplyr::filter(.data$values == TRUE)

  # Pull lengths vector
  if(nrow(res >= 1)){
    res2 <- res %>%
      dplyr::pull("lengths")

    # Calculate statistics
    if(stat == "max"){
      res3 <- res2 %>% max(na.rm = TRUE)
    } else if(stat == "min"){
      res3 <- res2 %>% min(na.rm = TRUE)
    } else if(stat == "mean"){
      res3 <- res2 %>% mean(na.rm = TRUE)
    } else if(stat == "median"){
      res3 <- res2 %>% median(na.rm = TRUE)
    } else if(stat == "sd"){
      res3 <- res2 %>% sd(na.rm = TRUE)
    } else if(stat == "var"){
      res3 <- res2 %>% var(na.rm = TRUE)
    }

  } else {
    res3 <- NA
  }

  # Return result
  return(res3)
}


