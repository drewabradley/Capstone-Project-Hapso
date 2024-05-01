library(httr)
library(jsonlite) 

# Function to query the API and retrieve vehicle data using httr
VehicleAPIrConnect <- function(VINin){
  url <- paste0("https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVinValues/", VINin, "?format=json")
  
  # Make the API request with httr
  response <- GET(url)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON content from the response
    content_from_api <- content(response, "text", encoding = "UTF-8")
    tempExtract <- fromJSON(content_from_api)
    
    # Convert the results to a data frame
    dfOut <- data.frame(t(unlist(tempExtract$Results)), stringsAsFactors=FALSE)
    return(dfOut)
  } else {
    stop(paste("API request failed with status code:", status_code(response)))
  }
}

# Function to decode VIN and only return make, model, and year
DecodeVIN <- function(VINin) {
  vehicleInfo <- VehicleAPIrConnect(VINin)
  # Select only the columns for Make, Model, and ModelYear
  requiredInfo <- vehicleInfo[, c("Make", "Model", "ModelYear")]
  requiredInfo
}

# Read CSV file with VINs using base R
vinData <- read.csv("nyc_vin.csv", stringsAsFactors = FALSE) # replace with your CSV file path

# Initialize an empty data frame to collect results
results <- data.frame(Make = character(), Model = character(), ModelYear = character(), stringsAsFactors = FALSE)

# Start processing from the 13,570th row
startRow <- 13570
totalRows <- nrow(vinData)

for (i in startRow:totalRows) { 
  VIN <- vinData$VIN[i]
  decodedInfo <- DecodeVIN(VIN)
  results <- rbind(results, decodedInfo)
  
  cat(sprintf("Processed %d out of %d rows.\n", i, totalRows))
  flush.console() # To print the message to the console immediately
}

# Write the result to a new CSV file
write.csv(results, "decoded_vins_continued.csv", row.names = FALSE)