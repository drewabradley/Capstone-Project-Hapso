library(RJSONIO)

# Function to query the API and retrieve vehicle data
VehicleAPIrConnect <- function(VINin){
  tempCall <- paste0("https://vpic.nhtsa.dot.gov/api/vehicles/DecodeVinValues/", VINin, "?format=json")
  tempExtract <- fromJSON(tempCall)
  dfOut <- data.frame(t(unlist(tempExtract$Results)), stringsAsFactors=FALSE)
  dfOut
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

# Loop through each VIN in the CSV and process it
totalRows <- nrow(vinData)
for (i in 1:totalRows) { 
  VIN <- vinData$VIN[i]
  decodedInfo <- DecodeVIN(VIN)
  results <- rbind(results, decodedInfo)
  
  # Print the progress message
    cat(sprintf("Processed %d out of %d rows.\n", i, totalRows))
    flush.console() # To print the message to the console immediately
  }
}

# Write the result to a new CSV file
write.csv(results, "decoded_vins.csv", row.names = FALSE)

