#' Calculate annual almond yield and profit from time series climate data
#'
#' @param climate_df_clean Data frame. Columns must include `year`, `month`, `tmin_c` (minimum temperature in Celsius), and `precip` (precipitation in millimeters).
#' @param almond_price Numeric. Market price per ton of almonds
#' @param cost_per_acre Numeric. Production costs per acre of almond agriculture
#'
#' @return Data Frame. Data Frame describing the year (year), almond yield (almond_yield) in tons/acre, and almond profit (profit) in USD
#'
calculate_yield_and_profit <- function(climate_df_clean, 
                             almond_price = 2000, 
                             cost_per_acre = 1000) {
  
  # Source almond yield function
  source(here("R", "almond-yield.R"))
  
  # Calculate almond yield
  yield_df <- calculate_almond_yield(climate_df_clean)
  
  # Calculate almond profit
  yield_df$profit <- round(yield_df$almond_yield * almond_price - cost_per_acre, 2)
  return(yield_df)
}