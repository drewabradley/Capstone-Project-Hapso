#Combine the two data sets

#Clean environment
rm(list=ls())

#SetWD
setwd("C:/Users/zachh/OneDrive/Documents/01 Spring 2024/BAIS Capstone/Capstone")

#load both csv
Penn_Crash_Data <- read.csv("Penn_Crash_Data_Cleaned.csv", header = TRUE)
Penn_Vehicle_Data <- read.csv("Penn_Vehicle_Data_Cleaned.csv", header = TRUE)

#Combine the dataframes based on the CRN, if no match, remove the row
Penn_Combined_VIN <- merge(Penn_Crash_Data, Penn_Vehicle_Data, by = "CRN", all = FALSE)

#Add one A and 6 0 to the end of the VIN to make it a full VIN
Penn_Combined_VIN$FULL_VIN <- paste0(Penn_Combined_VIN$PARTIAL_VIN, "A00000")

#Remove make, model, and partial VIN from the dataframe
Penn_Combined_VIN <- select(Penn_Combined_VIN, -c(MAKE_CD, MODEL_YR))

#Time to test the VIN API
#load libraries
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

# Initialize an empty dataframe to store results
results <- data.frame(CRN = integer(), Make = character(), Model = character(), ModelYear = character(), stringsAsFactors = FALSE)

# Iterate over each row in the dataframe
for (i in 1:nrow(Penn_Combined_VIN)) {
  VIN <- Penn_Combined_VIN$PARTIAL_VIN[i]
  CRN <- Penn_Combined_VIN$CRN[i]
  
  # Use tryCatch for error handling
  vehicle_info <- tryCatch({
    DecodeVIN(VIN)
  }, error = function(e) {
    # Return NA values if there is an error
    return(data.frame(Make = NA, Model = NA, ModelYear = NA))
  })
  
  # Add the CRN to the vehicle_info
  vehicle_info$CRN <- CRN
  
  # Append the results to the results dataframe
  results <- rbind(results, vehicle_info)
  
  # Print progress
  cat(sprintf("Processed %d out of %d rows.\n", i, nrow(Penn_Combined_VIN)))
  
}

# Merge the results with the original dataframe by CRN
final_df <- merge(Penn_Combined_VIN, results, by = "CRN", all.x = TRUE)

#Remove any rows from final_df with missing make
final_df <- final_df[!is.na(final_df$Make), ]

# You can now view or save final_df as needed
head(final_df)

#Save the final dataframe 
write.csv(final_df, "Penn_Combined_VIN.csv", row.names = FALSE)

#Clean up
#Load library for select
library(dplyr)
#load penn_combined_vin
Penn_Combined_VIN <- read.csv("Penn_Combined_VIN.csv", header = TRUE)

#Remove the make cd, model yR, and partial VIN
Penn_Combined_VIN <- select(Penn_Combined_VIN, -c(MAKE_CD, MODEL_YR, PARTIAL_VIN))

#Save final cleaned CSV
write.csv(Penn_Combined_VIN, "Final_Penn_Combined_VIN.csv", row.names = FALSE)


