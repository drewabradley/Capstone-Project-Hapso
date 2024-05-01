rm(list = ls())

install.packages(c("httr", "readr", "dplyr"))

library(httr)
library(readr)
library(dplyr)

# Ensure get_vehicle_info always returns a consistent data frame structure
get_vehicle_info <- function(vin) {
  url <- paste0("https://vpic.nhtsa.dot.gov/api/vehicles/decodevinvalues/", vin, "?format=json")
  response <- tryCatch({
    GET(url)
  }, error = function(e) {
    return(NULL) # In case of error, return NULL and handle it later
  })
  
  if (!is.null(response) && status_code(response) == 200) {
    content <- content(response, "parsed")
    result <- content$Results[[1]]
    return(tibble(
      VIN = vin,
      Year = as.character(result$ModelYear),
      Make = as.character(result$Make),
      Model = as.character(result$Model)
    ))
  } else {
    # Ensure that failed requests still return a row with NA values but consistent structure
    return(tibble(
      VIN = vin,
      Year = NA_character_,
      Make = NA_character_,
      Model = NA_character_
    ))
  }
}

# Improved function to process VINs with enhanced error handling
process_vin_file <- function(input_file, output_file) {
  vins <- read_csv(input_file, col_types = cols(VIN = col_character()))
  
  # Initialize an empty tibble for results
  results <- tibble(VIN = character(), Year = character(), Make = character(), Model = character())
  
  # Process each VIN and bind the result
  for (vin in vins$VIN) {
    vin_info <- get_vehicle_info(vin)
    results <- bind_rows(results, vin_info) # Use bind_rows for better handling of row binding
  }
  
  write_csv(results, output_file)
  message("Finished processing. Output saved to ", output_file)
}

# Example usage
input_file <- "Partical_VIN.csv" # Correct the filename if needed
output_file <- "decoded_vins.csv"

process_vin_file(input_file, output_file)
