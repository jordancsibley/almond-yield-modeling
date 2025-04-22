#' Calculate almond yield anomalies from time series climate data
#'
#' @param climate_data Data frame. Columns must include `year`, `month`, `tmin_c` (minimum temperature in Celsius), and `precip` (precipitation in millimeters).
#' @param coef_temp Numeric. Regression coefficient on February minimum temperature (Celcius).
#' @param coef_temp2 Numeric. Regression coefficient on February maximum temperature squared (Celcius).
#' @param coef_precip Numeric. Regression coefficient on January total precipitation (mm).
#' @param coef_precip2 Numeric. Regression coefficient on January total precipitation squared (mm).
#' @param intercept Numeric. Regression intercept term.
#'
#' @return List of maximum, minimum, and average almond yield anomalies in tons/acre for given time series. 
#'
#' @examples
#' yield_anomalies <- calculate_almond_yield(climate_data = df)
#' print(yield_anomalies)

calculate_almond_yield <- function(climate_data, coef_temp = -0.015, coef_temp2 = -0.0046, 
                                   coef_precip = -0.07, coef_precip2 = 0.0043, intercept = 0.28) {
  
  # Suppress group_by |> summarise messages
  options(dplyr.summarise.inform = FALSE)
  
  # Check that input data frame has required columns
  required_cols <- c("year", "month", "tmin_c", "precip")
  missing_cols <- setdiff(required_cols, colnames(climate_data))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  # Aggregate to minimum monthly temp and filter minimum temp column to February
  feb_temp <- climate_data |> 
    group_by(year, month) |> 
    summarise(min_temp = mean(tmin_c)) |> 
    ungroup() |> 
    filter(month == 2)
  
  # Aggregate to total monthly precip and filter precip column to January 
  jan_precip <- climate_data |> 
    group_by(year, month) |> 
    summarise(sum_precip = sum(precip)) |> 
    ungroup() |> 
    filter(month == 1)
  
  # Join filtered climate variables to single data frame 
  climate_prepped <- left_join(feb_temp, jan_precip, by = "year")
  
  # Calculate almond yield per year
  almond_yield_df <- climate_prepped |> 
    mutate(almond_yield = round(coef_temp * min_temp + 
                                  coef_temp2 * min_temp^2 + 
                                  coef_precip * sum_precip + 
                                  coef_precip2 * sum_precip^2 + 
                                  intercept, 
                                3)
    )
  
  
  # Return annual yield df
  return(almond_yield_df |>
           select(c(year, almond_yield)))
  
  # Return yield anomalies
  # return(c(
  #   max_yield = max(almond_yield_df$almond_yield),
  #   min_yield = min(almond_yield_df$almond_yield),
  #   mean_yield = mean(almond_yield_df$almond_yield)
  # ))
  
}
