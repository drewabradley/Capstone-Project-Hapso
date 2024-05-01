#Clean Penn crash data and group crash types

#Clean evnironment
rm(list=ls())

#SetWD
setwd("C:/Users/zachh/OneDrive/Documents/01 Spring 2024/BAIS Capstone/Capstone")

#Load csv
Penn_Crash_Data <- read.csv("CRASH_Statewide_2022.csv", header = TRUE)

#Remove the rows except the CRN, Collision type and County
Penn_Crash_Data <- Penn_Crash_Data[,c(1, 12, 15)]

#assign variables to the Crash Type
#find the unique values in crash type
unique(Penn_Crash_Data$COLLISION_TYPE)

#define a named vector that maps the collision type codes to their descriptions based on the provided data dictionary
collision_types <- setNames(
  c("Non-collision", "Rear-end", "Head-on", "Backing", "Angle", 
    "Sideswipe (same dir.)", "Sideswipe (Opposite dir.)", 
    "Hit fixed object", "Hit pedestrian", "Other/Unknown (Expired)", 
    "Other", "Unknown"),
  c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "98", "99")
)

#convert the 'COLLISION_TYPE' column to a factor using the collision types vector
Penn_Crash_Data$COLLISION_TYPE <- factor(Penn_Crash_Data$COLLISION_TYPE, levels = names(collision_types), labels = collision_types)

#check the unique values to see the descriptive labels
unique(Penn_Crash_Data$COLLISION_TYPE)

#Next, remove all the counties except for Philadelphia
#filter out to only keep the five counties in the immediate Philadelphia area
philly_area_county_codes <- c("09", "15", "23", "46", "67")  # Using strings assuming your data has county codes as strings
Penn_Crash_Data <- Penn_Crash_Data[Penn_Crash_Data$COUNTY %in% philly_area_county_codes, ]

#create a vector with the names of the counties
philly_area_county_names <- c("Bucks", "Chester", "Delaware", "Montgomery", "Philadelphia")

#replace the codes in the dataset with the names
Penn_Crash_Data$COUNTY <- factor(Penn_Crash_Data$COUNTY, levels = philly_area_county_codes, labels = philly_area_county_names)

#check the unique values to ensure the transformation is correct
unique(Penn_Crash_Data$COUNTY)

#save the cleaned data to a new CSV file
write.csv(Penn_Crash_Data, "Penn_Crash_Data_Cleaned.csv", row.names = FALSE)


#basic bar chart to show the collision types using ggplot
library(ggplot2)
#bar chart of collision types
ggplot(Penn_Crash_Data, aes(x = COLLISION_TYPE)) +
  geom_bar() +
  labs(title = "Collision Types in Philadelphia Area",
       x = "Collision Type",
       y = "Number of Crashes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


