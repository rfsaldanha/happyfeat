#' Compute year given a time reference to cut
#'
#' This function creates a new variable \code{year_cut} by computing if the year of a date if bellow or above a \code{cut} reference.
#'
#' A possible use case is to create a reference where the year  starts on April or on the 34 epidemiological week.
#'
#' @param data a \code{tsibble} object.
#' @param cut numeric. A month or a epidemiological week.
#' @param cut_type character. \code{month} or \code{epiweek}
#'
#' @return a \code{tsibble} object with \code{year_cut} variable.
#' @export
#'
#' @importFrom rlang .data
#'
#' @examples
#' example_data <- tsibble::tsibble(
#' cod = rep(1, 3653),
#' time = seq.Date(as.Date("2010-01-01"), as.Date("2020-01-01"), by = "day"),
#' count = runif(3653),
#' key = cod,
#' index = time
#' )
#'
#' example_data %>%
#' year_cut(cut = 35, cut_type = "epiweek")
#'
year_cut <- function(data, cut, cut_type){
  # Isolate time index variable
  time_var <- tsibble::index_var(data)

  # Check assertions
  checkmate::assert_class(x = data, classes = "tbl_ts")
  checkmate::assert_choice(x = cut_type, choices = c("month", "epiweek"))
  checkmate::assert_date(x = dplyr::pull(data, time_var))
  checkmate::assert_integerish(x = cut, lower = 1, upper = ifelse(cut_type == "month", 12, 53))

  # Create reference variable with month or epiweeks
  if(cut_type == "month"){
    res <- dplyr::mutate(
      .data = data,
      ref = lubridate::month(get(time_var))
    )
  } else if (cut_type == "epiweek"){
    res <- dplyr::mutate(
      .data = data,
      ref = lubridate::epiweek(get(time_var))
    )
  }

  # If the month/epiweek of a date is smaller than cut,
  # the year_cut is the current year less 1,
  # else, year_cut is the current year
  res %>% dplyr::mutate(
    year = lubridate::year(get(time_var))
  ) %>%
    dplyr::mutate(
      year_cut = dplyr::case_when(
        ref < {{cut}} ~ .data$year - 1,
        .default = .data$year
      )
    ) %>%
      dplyr::select(-"ref", -"year")
}


