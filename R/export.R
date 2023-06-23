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

  return(invisible(sheets))
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

  return(invisible(sheets))
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

  return(invisible(sheets))
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

  return(invisible(sheets))
}
