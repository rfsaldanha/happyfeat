year_cut <- function(.data, cut, cut_type){
  if(cut_type == "month"){
    res <- dplyr::mutate(
      .data = .data,
      ref = lubridate::month(time)
    )
  } else if (cut_type == "epiweek"){
    res <- dplyr::mutate(
      .data = .data,
      ref = lubridate::epiweek(time)
    )
  }

  mutate(
    .data = res,
    year = lubridate::year(time),
    year_cut = case_when(
      ref < cut ~ year - 1,
      .default = year
    )
  )

}


