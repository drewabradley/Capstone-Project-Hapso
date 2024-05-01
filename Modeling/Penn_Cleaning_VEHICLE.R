#Clean Vehicle data

#clean environment
rm(list=ls())

#SetWD
setwd("C:/Users/zachh/OneDrive/Documents/01 Spring 2024/BAIS Capstone/Capstone")

#Load libraries
library(dplyr)
library(ggplot2)

#Load CSV
Penn_Vehicle_Data <- read.csv("VEHICLE_Statewide_2022.csv", header = TRUE)

#remove the commercial vehicles (Column 4)
Penn_Vehicle_Data <- Penn_Vehicle_Data[Penn_Vehicle_Data$COMM_VEH == "N", ]

#Remove vehicle types except 1
Penn_Vehicle_Data <- Penn_Vehicle_Data[Penn_Vehicle_Data$VEH_TYPE == 1, ]

#Only keep the crashes with damage indicator of 1 or 2
Penn_Vehicle_Data <- Penn_Vehicle_Data[Penn_Vehicle_Data$DAMAGE_IND %in% c(1,2), ]

#set impact points to a part of the vehicle (1 = front bumper/headlight, 2 = front quarter panel/side mirror, 
# 5 = rear bumper/tail lights, 6 = rear bumper, 7 = rear bumper/tail light, 10 = front quarter panel/side mirror,
# 11 = front bumper/headlight, 12 = front bumper) 
Penn_Vehicle_Data <- Penn_Vehicle_Data[Penn_Vehicle_Data$IMPACT_POINT %in% c(1,2,5,6,7,10,11,12), ]

#add the new colum that shows what the impact points are
Penn_Vehicle_Data <- Penn_Vehicle_Data %>%
  mutate(IMPACT_POINT_NAME = case_when(
    IMPACT_POINT == 1 ~ "front bumper/headlight",
    IMPACT_POINT == 2 ~ "front quarter panel/side mirror",
    IMPACT_POINT == 5 ~ "rear bumper/tail lights",
    IMPACT_POINT == 6 ~ "rear bumper",
    IMPACT_POINT == 7 ~ "rear bumper/tail light",
    IMPACT_POINT == 10 ~ "front quarter panel/side mirror",
    IMPACT_POINT == 11 ~ "front bumper/headlight",
    IMPACT_POINT == 12 ~ "front bumper",
    TRUE ~ as.character(IMPACT_POINT)  # Default case to keep original value as character if not matched
  ))

#remove all columns except CRN, Make_CD, Make_Yr, Partial_vin, and Impact point name
Penn_Vehicle_Data <- select(Penn_Vehicle_Data, CRN, MAKE_CD, MODEL_YR, PARTIAL_VIN, IMPACT_POINT_NAME)


#Remove any partial vins that are missing
Penn_Vehicle_Data <- Penn_Vehicle_Data[Penn_Vehicle_Data$PARTIAL_VIN != "", ]

#Only model year 2010 and newer vehicles
Penn_Vehicle_Data <- Penn_Vehicle_Data[Penn_Vehicle_Data$MODEL_YR >= 2010, ]

#remove any N/A values in the model year column
Penn_Vehicle_Data <- Penn_Vehicle_Data[!is.na(Penn_Vehicle_Data$MODEL_YR), ]

#count the models
model_counts <- Penn_Vehicle_Data %>%
  group_by(MAKE_CD) %>%
  summarize(Count = n()) %>%
  ungroup() %>%
  arrange(desc(Count)) %>%
  top_n(5, Count)

#plot with ggplot2
ggplot(data = model_counts, aes(x = reorder(MAKE_CD, Count), y = Count)) +
  geom_bar(stat = "identity") +
  labs(x = "Model Code", y = "Count", title = "Top 5 Vehicle Models by Count") +
  theme_minimal() +
  coord_flip() 



#save Penn_Vehicle_Data to a csv
write.csv(Penn_Vehicle_Data, "Penn_Vehicle_Data_Cleaned.csv", row.names = FALSE)

