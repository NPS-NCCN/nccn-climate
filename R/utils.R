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

  if (nrow(ts_info) == 0) {
    stop("No data found for the park, parameter, and labels that you requested.")
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
      dplyr::summarize(dplyr::across(param_col, summary_fun, .names = "{.col}")) %>%
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

#' Export daily precip and air temp summaries
#'
#' @inheritParams summarizeParkClimateData
#' @param file_out Path (including filename) of the .xlsx spreadsheet you want to create. Defaults to a file named with the park code and water year in the current working directory.
#' @param overwrite If `file_out` already exists, do you want to overwrite it? Defaults to `FALSE`.
#'
#' @return Writes data to `file_out` and returns a list containing the data that was written.
#' @export
#'
#' @examples
#' \dontrun{
#' exportNCCNDailySummaries("OLYM", 2022, "exports/OLYM_Daily_2022.xlsx")
#' }
exportNCCNDailySummaries <- function(park_code = c("MORA", "NOCA", "OLYM", "SAJH"), water_year, file_out = paste0("./", park_code, "_", water_year, "_daily.xlsx"), overwrite = FALSE) {
  park_code <- rlang::arg_match(park_code)
  if (missing(water_year)) {
    stop("Water year is required")
  }

  # Mean of hourly temperatures, broken down by day, month, and year
  avg_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Air Temp",
                                                    label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                    water_year = water_year,
                                                    summary = "mean",
                                                    summary_period = "day",
                                                    period_of_record = FALSE) %>%
                             c_to_f(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })

  # Maximum of hourly temperatures, broken down by day, month, and year
  max_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Air Temp",
                                                    label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                    water_year = water_year,
                                                    summary = "max",
                                                    summary_period = "day",
                                                    period_of_record = FALSE) %>%
                             c_to_f(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })

  # Minimum of hourly temperatures, broken down by day, month, and year
  min_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Air Temp",
                                                    label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                    water_year = water_year,
                                                    summary = "min",
                                                    summary_period = "day",
                                                    period_of_record = FALSE) %>%
                             c_to_f(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })

  # Sum of hourly precipitation, broken down by day, month, and year
  total_precip <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Precip Increm",
                                                    label = c("Rainfall", "Total Hourly", "RNIN", "NWAC-CSV"),
                                                    water_year = water_year,
                                                    summary = "sum",
                                                    summary_period = "day",
                                                    period_of_record = FALSE) %>%
                             mm_to_in(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })

  sheets <- list(AirTemp_Average_F = avg_air_temp,
                 Temperature_Max_Average_F = max_air_temp,
                 Temparature_Min_Average_F = min_air_temp,
                 Precipitation_Total_Inches = total_precip)

  raw_data_metadata <- purrr::map(sheets, ~ attr(.x, "metadata")) %>%
    purrr::reduce(rbind)

  calcs <- tibble::tibble(sheet = names(sheets),
                          calculation = c("Mean of hourly temperatures, broken down by day, month, and year",
                                          "Maximum of hourly temperatures, broken down by day, month, and year",
                                          "Minimum of hourly temperatures, broken down by day, month, and year",
                                          "Sum of hourly precipitation, broken down by day, month, and year"
                          ))

  sheets[["Calculations"]] <- calcs
  sheets[["Raw_Data_Metadata"]] <- raw_data_metadata

  openxlsx::write.xlsx(sheets, file_out, overwrite = overwrite)

  return(sheets)
}

#' Export monthly precip and air temp summaries
#'
#' @inheritParams summarizeParkClimateData
#' @param file_out Path (including filename) of the .xlsx spreadsheet you want to create. Defaults to a file named with the park code and water year in the current working directory.
#' @param overwrite If `file_out` already exists, do you want to overwrite it? Defaults to `FALSE`.
#'
#' @return Writes data to `file_out` and returns a list containing the data that was written.
#' @export
#'
#' @examples
#' \dontrun{
#' exportNCCNDailySummaries("OLYM", 2022, "exports/OLYM_Daily_2022.xlsx")
#' }
exportNCCNMonthlySummaries <- function(park_code = c("MORA", "NOCA", "OLYM", "SAJH"), water_year, file_out = paste0("./", park_code, "_", water_year, "_monthly.xlsx"), overwrite = FALSE) {
  park_code <- rlang::arg_match(park_code)
  if (missing(water_year)) {
    stop("Water year is required")
  }

  # Average of all hourly temperatures, broken down by month and year
  avg_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Air Temp",
                                                    label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                    water_year = water_year,
                                                    summary = "mean",
                                                    summary_period = "month",
                                                    period_of_record = FALSE) %>%
                             c_to_f(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })
  # Average of all daily maximum temperatures, broken down by month and year
  avg_max_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                        parameter = "Air Temp",
                                                        label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                        water_year = water_year,
                                                        summary = "max",
                                                        summary_period = "day",
                                                        period_of_record = FALSE) %>%
                                 c_to_f() %>%
                                 dplyr::mutate(Year = lubridate::year(Date),
                                                            Month = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>%
                                 dplyr::select(-Day, -Date) %>%
                                 dplyr::group_by(Year, Month) %>%
                                 dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(., na.rm = TRUE))) %>%
                                 dplyr::ungroup(),
                               error = function(e) {
                                 warning(e)
                                 return(tibble::tibble())
                               })

  # Average of all daily minimum temperatures, broken down by month and year
  avg_min_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                        parameter = "Air Temp",
                                                        label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                        water_year = water_year,
                                                        summary = "min",
                                                        summary_period = "day",
                                                        period_of_record = FALSE) %>%
                                 c_to_f() %>%
                                 dplyr::mutate(Year = lubridate::year(Date),
                                               Month = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>%
                                 dplyr::select(-Day, -Date) %>%
                                 dplyr::group_by(Year, Month) %>%
                                 dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(., na.rm = TRUE))) %>%
                                 dplyr::ungroup(),
                               error = function(e) {
                                 warning(e)
                                 return(tibble::tibble())
                               })

  # Maximum hourly temperature, broken down by month and year
  max_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Air Temp",
                                                    label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                    water_year = water_year,
                                                    summary = "max",
                                                    summary_period = "month",
                                                    period_of_record = FALSE) %>%
                             c_to_f(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })

  # Maximum hourly temperature, broken down by month and year
  min_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Air Temp",
                                                    label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                    water_year = water_year,
                                                    summary = "min",
                                                    summary_period = "month",
                                                    period_of_record = FALSE) %>%
                             c_to_f(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })

  # Sum of hourly precipitation, broken down by month and year
  total_precip <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Precip Increm",
                                                    label = c("Rainfall", "Total Hourly", "RNIN", "NWAC-CSV"),
                                                    water_year = water_year,
                                                    summary = "sum",
                                                    summary_period = "month",
                                                    period_of_record = FALSE) %>%
                             mm_to_in(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })

  sheets <- list(AirTemp_Average_F = avg_air_temp,
                 Temperature_Max_Average_F = avg_max_air_temp,
                 Temparature_Min_Average_F = avg_min_air_temp,
                 Temperature_Max_F = max_air_temp,
                 Temperature_Min_F = min_air_temp,
                 Precipitation_Total_Inches = total_precip)

  raw_data_metadata <- purrr::map(sheets, ~ attr(.x, "metadata")) %>%
    purrr::reduce(rbind)

  calcs <- tibble::tibble(sheet = names(sheets),
                          calculation = c("Average of all hourly temperatures, broken down by month and year",
                                          "Average of all daily maximum temperatures, broken down by month and year",
                                          "Average of all daily minimum temperatures, broken down by month and year",
                                          "Maximum hourly temperature, broken down by month and year",
                                          "Maximum hourly temperature, broken down by month and year",
                                          "Sum of hourly precipitation, broken down by month and year"
                          ))

  sheets[["Calculations"]] <- calcs
  sheets[["Raw_Data_Metadata"]] <- raw_data_metadata

  openxlsx::write.xlsx(sheets, file_out, overwrite = overwrite)

  return(sheets)
}

#' Export daily period of record summaries for precip and air temp
#'
#' @inheritParams summarizeParkClimateData
#' @param file_out Path (including filename) of the .xlsx spreadsheet you want to create. Defaults to a file named with the park code and water year in the current working directory.
#' @param overwrite If `file_out` already exists, do you want to overwrite it? Defaults to `FALSE`.
#'
#' @return Writes data to `file_out` and returns a list containing the data that was written.
#' @export
#'
#' @examples
#' \dontrun{
#' exportNCCNDailyPeriodOfRecord("OLYM", 2022, "exports/OLYM_Daily_2022.xlsx")
#' }
exportNCCNDailyPeriodOfRecord <- function(park_code = c("MORA", "NOCA", "OLYM", "SAJH"), file_out = paste(park_code, "daily_periodofrecord.xlsx", sep = "_"), overwrite = FALSE) {
  park_code <- rlang::arg_match(park_code)

  # average of all hourly temperatures across years, broken down by day of year (1-365)
  avg_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Air Temp",
                                                    label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                    summary = "mean",
                                                    summary_period = "day",
                                                    period_of_record = TRUE) %>%
                             c_to_f(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })

  # Maximum of hourly temperatures, across years, broken down by day of year
  max_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                        parameter = "Air Temp",
                                                        label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                        summary = "max",
                                                        summary_period = "day",
                                                        period_of_record = TRUE) %>%
                                 c_to_f(),
                               error = function(e) {
                                 warning(e)
                                 return(tibble::tibble())
                               })
  # Minimum of hourly temperatures, across years, broken down by day of year
  min_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                        parameter = "Air Temp",
                                                        label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                        summary = "min",
                                                        summary_period = "day",
                                                        period_of_record = TRUE) %>%
                                 c_to_f(),
                               error = function(e) {
                                 warning(e)
                                 return(tibble::tibble())
                               })

  total_precip <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Precip Increm",
                                                    label = c("Rainfall", "Total Hourly", "RNIN", "NWAC-CSV"),
                                                    summary = "sum",
                                                    summary_period = "day",
                                                    period_of_record = FALSE) %>%
                             mm_to_in(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })

  if (nrow(total_precip) > 0) {
    # Mean of daily total precip, across all years, broken down by day of year
    avg_total_precip <- total_precip %>%
      dplyr::select(-Date) %>%
      dplyr::group_by(Day) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(., na.rm = TRUE))) %>%
      dplyr::ungroup()

    # Max of daily total precip, across all years, broken down by day of year
    max_total_precip <- total_precip %>%
      dplyr::select(-Date) %>%
      dplyr::group_by(Day) %>%
      dplyr::summarise(dplyr::across(dplyr::everything(), ~ max(., na.rm = TRUE))) %>%
      dplyr::ungroup()
  } else {
    avg_total_precip <- tibble::tibble()
    max_total_precip <- tibble::tibble()
  }


  sheets <- list(AirTemp_Average_F = avg_air_temp,
                 Temperature_Max_Average_F = max_air_temp,
                 Temparature_Min_Average_F = min_air_temp,
                 Precipitation_Avg_Total_Inches = avg_total_precip,
                 Precip_Max_Total_Inches = avg_total_precip
  )

  raw_data_metadata <- purrr::map(sheets, ~ attr(.x, "metadata")) %>%
    purrr::reduce(rbind)
  calcs <- tibble::tibble(sheet = names(sheets),
                          calculation = c("Average of all hourly temperatures across years, broken down by day of year (1-365)",
                                          "Maximum of hourly temperatures, across years, broken down by day of year",
                                          "Minimum of hourly temperatures, across years, broken down by day of year",
                                          "Mean of daily total precip, across all years, broken down by day of year",
                                          "Max of daily total precip, across all years, broken down by day of year"
                          ))

  sheets[["Calculations"]] <- calcs
  sheets[["Raw_Data_Metadata"]] <- raw_data_metadata

  openxlsx::write.xlsx(sheets, file_out, overwrite = overwrite)

  return(sheets)
}

#' Export monthly period of record summaries for precip and air temp
#'
#' @inheritParams summarizeParkClimateData
#' @param file_out Path (including filename) of the .xlsx spreadsheet you want to create. Defaults to a file named with the park code and water year in the current working directory.
#' @param overwrite If `file_out` already exists, do you want to overwrite it? Defaults to `FALSE`.
#'
#' @return Writes data to `file_out` and returns a list containing the data that was written.
#' @export
#'
#' @examples
#' \dontrun{
#' exportNCCNMonthlyPeriodOfRecord("OLYM", 2022, "exports/OLYM_Daily_2022.xlsx")
#' }
exportNCCNMonthlyPeriodOfRecord <- function(park_code = c("MORA", "NOCA", "OLYM", "SAJH"), file_out = paste(park_code, "monthly_periodofrecord.xlsx", sep = "_"), overwrite = FALSE) {
  park_code <- rlang::arg_match(park_code)

  # Mean hourly air temp, across all years, broken down by month
  avg_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Air Temp",
                                                    label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                    summary = "mean",
                                                    summary_period = "month",
                                                    period_of_record = TRUE) %>%
                             c_to_f(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })

  # Average of daily maximum temperatures, across all years, broken down by month
  avg_max_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                        parameter = "Air Temp",
                                                        label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                        summary = "max",
                                                        summary_period = "day",
                                                        period_of_record = FALSE) %>%
                                 c_to_f() %>%
                                 dplyr::mutate(Month = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>%
                                 dplyr::select(-Day, -Date) %>%
                                 dplyr::group_by(Month) %>%
                                 dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(., na.rm = TRUE))) %>%
                                 dplyr::ungroup(),
                               error = function(e) {
                                 warning(e)
                                 return(tibble::tibble())
                               })

  # Average of daily minimum temperatures, across all years, broken down by month
  avg_min_air_temp <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                        parameter = "Air Temp",
                                                        label = c("Avg", "NWAC-CSV", "ATC", "Current"),
                                                        summary = "min",
                                                        summary_period = "day",
                                                        period_of_record = FALSE) %>%
                                 c_to_f() %>%
                                 dplyr::mutate(Month = lubridate::month(Date, label = TRUE, abbr = FALSE)) %>%
                                 dplyr::select(-Day, -Date) %>%
                                 dplyr::group_by(Month) %>%
                                 dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(., na.rm = TRUE))) %>%
                                 dplyr::ungroup(),
                               error = function(e) {
                                 warning(e)
                                 return(tibble::tibble())
                               })
  # Average of total monthly precipitation across all years, broken down by month
  total_precip <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Precip Increm",
                                                    label = c("Rainfall", "Total Hourly", "RNIN", "NWAC-CSV"),
                                                    summary = "sum",
                                                    summary_period = "month",
                                                    period_of_record = FALSE) %>%
                             mm_to_in() %>%
                             dplyr::select(-Year) %>%
                             dplyr::group_by(Month) %>%
                             dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(., na.rm = TRUE))) %>%
                             dplyr::ungroup(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })

  # Maximum of total monthly precipitation across all years, broken down by month
  max_precip <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Precip Increm",
                                                    label = c("Rainfall", "Total Hourly", "RNIN", "NWAC-CSV"),
                                                    summary = "sum",
                                                    summary_period = "month",
                                                    period_of_record = FALSE) %>%
                             mm_to_in() %>%
                           dplyr::select(-Year) %>%
                           dplyr::group_by(Month) %>%
                           dplyr::summarise(dplyr::across(dplyr::everything(), ~ max(., na.rm = TRUE))) %>%
                           dplyr::ungroup(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })

  # Maximum of total monthly precipitation across all years, broken down by month
  min_precip <- tryCatch(summarizeParkClimateData(park_code = park_code,
                                                    parameter = "Precip Increm",
                                                    label = c("Rainfall", "Total Hourly", "RNIN", "NWAC-CSV"),
                                                    summary = "sum",
                                                    summary_period = "month",
                                                    period_of_record = FALSE) %>%
                             mm_to_in() %>%
                           dplyr::select(-Year) %>%
                           dplyr::group_by(Month) %>%
                           dplyr::summarise(dplyr::across(dplyr::everything(), ~ min(., na.rm = TRUE))) %>%
                           dplyr::ungroup(),
                           error = function(e) {
                             warning(e)
                             return(tibble::tibble())
                           })

  sheets <- list(AirTemp_Average_F = avg_air_temp,
                 Temperature_Max_Average_F = avg_max_air_temp,
                 Temparature_Min_Average_F = avg_min_air_temp,
                 Precipitation_Total_Avg_Inches = total_precip,
                 Precip_Max_Inches = max_precip,
                 Precip_Min_Inches = min_precip
                 )

  raw_data_metadata <- purrr::map(sheets, ~ attr(.x, "metadata")) %>%
    purrr::reduce(rbind)
  calcs <- tibble::tibble(sheet = names(sheets),
                         calculation = c("Mean hourly air temp, across all years, broken down by month",
                                         "Average of daily maximum temperatures, across all years, broken down by month",
                                         "Average of daily minimum temperatures, across all years, broken down by month",
                                         "Average of total monthly precipitation across all years, broken down by month",
                                         "Maximum of total monthly precipitation across all years, broken down by month",
                                         "Maximum of total monthly precipitation across all years, broken down by month"
                         ))

  sheets[["Calculations"]] <- calcs
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
  names(df) <- stringr::str_replace(names(df), "(_mm)|(_in)", "")
  return(df)
}
