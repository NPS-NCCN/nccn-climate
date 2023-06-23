#' @importFrom magrittr %>% %<>%

#' @importFrom fetchaquarius connectToAquarius
#' @export
fetchaquarius::connectToAquarius

#' @importFrom fetchaquarius disconnectFromAquarius
#' @export
fetchaquarius::disconnectFromAquarius

#' Get Water Year
#'
#' Computes the water year that a date falls in, where the water year runs from
#' Oct 1 - Sept 30.
#'
#' @param date A date-time object
#'
#' @return The water year of `date` as an integer number
#' @export
#'
#' @examples
#' my_date <- as.Date("2022-02-01")
#' getWaterYear(my_date)  # 2022
#' another_date <- as.Date("2022-10-01") # 2023
getWaterYear <- function(date) {
  c_year <- lubridate::year(date)
  month <- lubridate::month(date)
  if (month >= 10) {
    w_year <- c_year + 1
  } else {
    w_year <- c_year
  }

  return(w_year)
}

c_to_f <- function(df) {
  df <- dplyr::mutate(df, dplyr::across(dplyr::ends_with("_degC"), ~ . * 9/5 + 32))
  names(df) <- stringr::str_replace(names(df), "_deg[C|F]", "")
  return(df)
}

mm_to_in <- function(df) {
  df <- dplyr::mutate(df, dplyr::across(dplyr::ends_with("_mm"), ~ . / 25.4))
  names(df) <- stringr::str_replace(names(df), "(_mm)|(_in)", "")
  return(df)
}
