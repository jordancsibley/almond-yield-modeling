# Insert function code 
calculate_almond_yield <- function() {

  # Filter minimum temp column to February
  feb_temp <- data_clean |> 
    group_by(year, month) |> 
    summarise(min_temp = min(tmin_c)) |> 
    ungroup() |> 
    filter(month == 2) |> 
    select(-month)
  
  # Filter precip column to January 
  jan_precip <- data_clean |> 
    group_by(year, month) |> 
    summarise(sum_precip = sum(precip)) |> 
    ungroup() |> 
    filter(month == 1) |> 
    select(-month)
  
  # Join filtered climate variables to single data frame 
  climate_input <- left_join(feb_temp, jan_precip, by = "year")
  
  # Calculate almond yield per year
  almond_yield_df <- climate_input |> 
    mutate(almond_yield = 
             round((-0.015 * min_temp) - (0.0046 * (min_temp ^ 2)) - (0.07 * sum_precip) + (0.0043 * (sum_precip ^2)) + 0.28, 3)
    )
  
  # Return yield anomalies
  # return(
  #   print("Maximum almond yield (tons/year)", max(almond_yield_df$almond_yield), 
  #         "\n Minimum almond yield (tons/year)", min(almond_yield_df$almond_yield),
  #         "\n Minimum almond yield (tons/year)", min(almond_yield_df$almond_yield),)
  #   
  #)
  
}