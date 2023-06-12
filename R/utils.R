#' @importFrom magrittr %>% %<>%

#' @importFrom fetchaquarius connectToAquarius
#' @export
fetchaquarius::connectToAquarius

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

#' Get NCCN climate time series information
#'
#' Retrieves information about climate time series datasets for NCCN parks (MORA, NOCA, OLYM, and SAJH)
#'
#' @param park_code Four-letter park code(s)
#' @param include_derived Include derived time series?
#'
#' @return A tibble with one line per dataset
#' @export
#'
#' @examples
#' \dontrun{
#' olym_ts_info <- getClimateTimeSeriesInfo("OLYM")
#' }
getClimateTimeSeriesInfo <- function(park_code = c("MORA", "NOCA", "OLYM", "SAJH"), include_derived = FALSE) {
  park_code <- rlang::arg_match(park_code, multiple = TRUE)  # Validate park code

  climate_locs <- getClimateStationList(park_code)
  ts_info <- fetchaquarius::getTimeSeriesInfo(climate_locs$Identifier) %>%
    dplyr::arrange(LocationIdentifier, Parameter, Label) %>%
    dplyr::select(LocationIdentifier, Identifier, Parameter, Label, Unit, Comment, Description, ComputationIdentifier, ComputationPeriodIdentifier, LastModified, TimeSeriesType) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(.x, "")))

  if (!include_derived) {
    ts_info <- dplyr::filter(ts_info, TimeSeriesType == "ProcessorBasic")
  }

  return(ts_info)
}

#' Get a list of valid parameters and labels
#'
#' Lists all valid parameters and labels for each climate station at a park
#'
#' @inheritParams getClimateTimeSeriesInfo
#'
#' @return A tibble with one line per dataset
#' @export
#'
#' @examples
#' \dontrun{
#' olym_params <- getClimateParams("OLYM")
#' }
getClimateParams <- function(park_code = c("MORA", "NOCA", "OLYM", "SAJH"), include_derived = FALSE) {

  param_info <- getClimateTimeSeriesInfo(park_code, include_derived) %>%
    dplyr::mutate(Derived = TimeSeriesType != "ProcessorBasic") %>%
    dplyr::select(LocationIdentifier, Parameter, Derived, Label) %>%
    tidyr::pivot_wider(names_from = LocationIdentifier, values_from = Label, values_fn = ~ paste(.x, collapse = ", ")) %>%
    dplyr::arrange(Parameter, Derived)

  return(param_info)
}


#' Get information about NCCN climate stations
#'
#' Retrieves information about climate stations for NCCN parks (MORA, NOCA, OLYM, and SAJH)
#'
#' @param park_code Four-letter park code(s)
#'
#' @return A tibble with one line per climate station
#' @export
#'
#' @examples
#' \dontrun{
#' olym_station_info <- getClimateStationList("OLYM")
#' nccn_station_info <- getClimateStationList()
#' }
getClimateStationList <- function(park_code = c("MORA", "NOCA", "OLYM", "SAJH")) {
  park_code <- rlang::arg_match(park_code, multiple = TRUE)  # Validate park code

  climate_folders <- paste0("National Park Service.North Coast and Cascades Network.", park_code, "_Climate")
  climate_stations <- fetchaquarius::getLocationInfo(folder = "National Park Service.North Coast and Cascades Network") %>%
    dplyr::filter(PrimaryFolder %in% climate_folders) %>%
    dplyr::select(Name, Identifier, LastModified, Publish, PrimaryFolder) %>%
    dplyr::arrange(PrimaryFolder, Identifier)

  return(climate_stations)
}

#' Get all corrected climate time series data for a park
#'
#' Retrieves all corrected, unsummarized time series data from the \[PARK\]_Climate folder in Aquarius (e.g. OLYM_Climate).
#'
#' @param park_code Four-letter park code(s)
#' @param parameter A character string indicating which parameter to retrieve (e.g. "Air Temp").
#' @param water_year Integer vector indicating the water year(s) to fetch data for
#' @param time_zone String indicating the time zone the data were collected in. See the full list of [TZ identifiers](https://en.wikipedia.org/wiki/List_of_tz_database_time_zones) for more info.
#' @inheritParams fetchaquarius::getTimeSeries
#' @inheritParams getClimateTimeSeriesInfo
#'
#' @return A list of corrected, unsummarized time series by park
#' @export
#'
#' @examples
#' \dontrun{
#' olym <- getParkClimateData("OLYM")
#'
#' }
getParkClimateData <- function(park_code = c("MORA", "NOCA", "OLYM", "SAJH"), parameter, labels, water_year, include_derived = FALSE, time_zone = "America/Los_Angeles") {

  if (length(parameter) > 1) {
    stop("This function can only retrieve data for one parameter at a time.")
  }

  if (missing(parameter)) {
    stop("You must choose a single parameter to retrieve data for.")
  }

  if (!missing(water_year)) {
    water_year <- as.integer(water_year)
    y_start <- min(water_year) - 1
    y_end <- max(water_year)
    start <- lubridate::force_tz(lubridate::as_datetime(paste0(y_start, "-10-01T00:00:00")), time_zone)
    end <- lubridate::force_tz(lubridate::as_datetime(paste0(y_end, "-09-30T23:59:59")), time_zone)
  }
  ts_info <- getClimateTimeSeriesInfo(park_code, include_derived = include_derived) %>%
    dplyr::filter(tolower(Parameter) == tolower(parameter),
                  LocationIdentifier != "NOCA_21a37s")  # Exclude weird location

  if (!missing(labels)) {
    ts_info <- dplyr::filter(ts_info, tolower(Label) %in% tolower(labels))
  }

  all_ts_data <- list()
  for (i in 1:nrow(ts_info)) {
    loc_id <- ts_info$LocationIdentifier[i]
    label <- ts_info$Label[i]
    ts_id <- ts_info$Identifier[i]
    if (!missing(water_year)) {
      ts_data <- fetchaquarius::getTimeSeries(ts_id, start, end)
    } else {
      ts_data <- fetchaquarius::getTimeSeries(ts_id)
    }

    param_name <- paste(janitor::make_clean_names(ts_data$Parameter, replace = c(`'` = "", `"` = "", `%` = "_percent_", `#` = "_number_", "24" = "twentyfour")),
                        ts_data$Unit, sep = "_")
    ts <- tibble::tibble(timestamp = lubridate::with_tz(ts_data$Points$Timestamp, time_zone),
                         Param = ts_data$Points$Value$Numeric)
    names(ts)[2] <- param_name

    ts_meta <- tibble::tibble(LocationIdentifier = ts_data$LocationIdentifier,
                              Parameter = ts_data$Parameter,
                              Label = ts_data$Label,
                              Unit = ts_data$Unit,
                              RecordCount = ts_data$NumPoints,
                              DateRetrieved = lubridate::with_tz(ts_data$ResponseTime, time_zone),
                              DateRangeStart = lubridate::with_tz(ts_data$TimeRange$StartTime, time_zone),
                              DateRangeEnd =  lubridate::with_tz(ts_data$TimeRange$EndTime, time_zone)
                              )
    attr(ts, "metadata") <- ts_meta
    all_ts_data[[parameter]][[label]][[loc_id]] <- ts
  }

  return(all_ts_data)
}

#' Summarize NCCN Climate Data
#'
#' @param summary Type of summary to perform. Options are "mean", "min", "max", and "sum".
#' @param summary_period Period over which to summarize. Options are "day" and "month".
#' @param period_of_record Summarize over the whole period of record? If `TRUE`, summarizes across all years in dataset. If `FALSE`, summaries are grouped by year.
#' @inheritParams getParkClimateData
#'
#' @return A data frame with a column of data for each station
#' @export
#'
#' @examples
#' \dontrun{
#' # Summarize monthly total precip at OLYM over entire period of record
#' olym_precip <- summarizeParkClimateData("OLYM", "Precip Increm", "Rainfall", summary = "sum", summary_period = "month", period_of_record = TRUE)
#' olym_precip_2022 <- summarizeParkClimateData("OLYM", "Precip Increm", "Rainfall", 2022, summary = "sum", summary_period = "month", period_of_record = FALSE)
#' }
summarizeParkClimateData <- function(park_code = c("MORA", "NOCA", "OLYM", "SAJH"), parameter, label, water_year, summary = c("mean", "min", "max", "sum"), summary_period = c("day", "month"), period_of_record = FALSE, time_zone = "America/Los_Angeles") {
  summary <- rlang::arg_match(summary, multiple = FALSE)
  summary_period <- rlang::arg_match(summary_period, multiple = FALSE)

  if (summary == "mean") {
    summary_fun <- function(x) {mean(x, na.rm = TRUE)}
  } else if (summary == "min") {
    summary_fun <- function(x) {min(x, na.rm = TRUE)}
  } else if (summary == "max") {
    summary_fun <- function(x) {max(x, na.rm = TRUE)}
  } else if (summary == "sum") {
    summary_fun <- function(x) {sum(x, na.rm = TRUE)}
  }

  map_fun <- function(df_name) {
    df <- raw_data[[df_name]]  # get data frame
    units <- attr(df, "metadata")$Unit  # get units
    param_col <- paste(df_name, units, sep = "_")  # rename parameter column to station name, but add units
    names(df)[2] <- param_col

    if (summary_period == "day") {  # Summarize by day
      if (!period_of_record) {
        df <- df %>% dplyr::mutate(Date = format(timestamp, "%Y-%m-%d %Z"),
                                   Day = lubridate::yday(timestamp))
        grp_by <<- c("Date", "Day")  # Group by date and day of year
      } else {
        df <- df %>% dplyr::mutate(Day = lubridate::yday(timestamp))
        grp_by <<- "Day"  # Group by day of year only for period of record calculations
      }

    } else if (summary_period == "month") {  # Summarize by month
      if (!period_of_record) {
        df <- df %>% dplyr::mutate(Year = lubridate::year(timestamp),
                                   Month = lubridate::month(timestamp, label = TRUE, abbr = FALSE))
        grp_by <<- c("Year", "Month")  # Group by year and month
      } else {
        df <- df %>% dplyr::mutate(Month = lubridate::month(timestamp, label = TRUE, abbr = FALSE))
        grp_by <<- "Month"  # Group by month only for period of record
      }
    }
    df <- df %>%
      dplyr::select(-timestamp) %>%
      dplyr::group_by(dplyr::pick(grp_by)) %>%
      dplyr::summarize(dplyr::across(param_col, summary_fun)) %>%
      dplyr::ungroup()

    return(df)
  }

  raw_data <- getParkClimateData(park_code, parameter, label, water_year, time_zone = time_zone)
  raw_data <- raw_data[[1]][[1]]  # Only one parameter and label so we don't need all this nesting
  summarized_data <- purrr::map(names(raw_data), map_fun) %>%
    purrr::reduce(dplyr::full_join, by = grp_by)
  summarized_meta <- purrr::map(raw_data, function(df) {attr(df, "metadata")}) %>%
    purrr::reduce(rbind)
  attr(summarized_data, "metadata") <- summarized_meta

  return(summarized_data)
}

exportNCCNDailySummaries <- function(park_code = c("MORA", "NOCA", "OLYM", "SAJH"), water_year, file_out, overwrite = FALSE) {
  avg_air_temp <- summarizeParkClimateData(park_code = park_code,
                                        parameter = "Air Temp",
                                        label = "Avg",
                                        water_year = water_year,
                                        summary = "mean",
                                        summary_period = "day",
                                        period_of_record = FALSE) %>%
    c_to_f()

  max_air_temp <- summarizeParkClimateData(park_code = park_code,
                                           parameter = "Air Temp",
                                           label = "Avg",
                                           water_year = water_year,
                                           summary = "max",
                                           summary_period = "day",
                                           period_of_record = FALSE) %>%
    c_to_f()

  min_air_temp <- summarizeParkClimateData(park_code = park_code,
                                           parameter = "Air Temp",
                                           label = "Avg",
                                           water_year = water_year,
                                           summary = "min",
                                           summary_period = "day",
                                           period_of_record = FALSE) %>%
    c_to_f()

  total_precip <- summarizeParkClimateData(park_code = park_code,
                                           parameter = "Precip Increm",
                                           label = "Rainfall",
                                           water_year = water_year,
                                           summary = "sum",
                                           summary_period = "day",
                                           period_of_record = FALSE) %>%
    mm_to_in()

  sheets <- list(AirTemp_Average_F = avg_air_temp,
                 Temperature_Max_Average_F = max_air_temp,
                 Temparature_Min_Average_F = min_air_temp,
                 Precipitation_Total_Inches = total_precip)

  raw_data_metadata <- purrr::map(sheets, ~ attr(.x, "metadata")) %>%
    purrr::reduce(rbind)

  sheets[["Raw_Data_Metadata"]] <- raw_data_metadata

  openxlsx::write.xlsx(sheets, file_out, overwrite = overwrite)

  return(sheets)
}

c_to_f <- function(df) {
  df <- dplyr::mutate(df, dplyr::across(dplyr::ends_with("_degC"), ~ . * 9/5 + 32))
  names(df) <- stringr::str_replace(names(df), "_deg[C|F]", "")
  return(df)
}

mm_to_in <- function(df) {
  df <- dplyr::mutate(df, dplyr::across(dplyr::ends_with("_mm"), ~ . / 25.4))
  names(df) <- stringr::str_replace(names(df), "(mm)|(in)", "")
  return(df)
}
