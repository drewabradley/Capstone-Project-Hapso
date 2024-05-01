rm(list=ls())

getwd()
setwd("C:/Users/Drewb/Downloads")

library(dplyr)
library(ggmap)

rm(list=ls())

PA_FLAG <- read.csv("FLAG_Statewide_2022.csv")
PA_VEHICLE <- read.csv("VEHICLE_Statewide_2022.csv")
PA_CRASH <- read.csv("CRASH_Statewide_2022.csv")

PAcrashes <- merge(PA_FLAG, PA_CRASH, by = "CRN")
PAcrashes <- merge(PAcrashes, PA_VEHICLE, by = "CRN")



PAcrashes <- subset(PAcrashes, select = c("CRN", "MAKE_CD", "MODEL_YR", "COLLISION_TYPE", "DEC_LAT", "DEC_LONG", "ANGLE_CRASH", "REAR_END", "DAMAGE_IND", "PARTIAL_VIN"))
View(PAcrashes)

#FILTERING TO ONLY CRASHED IN PHILADELPHIA
latitude_min <- 39.867
latitude_max <- 40.14
longitude_min <- -75.28
longitude_max <- -75.00

PHIcrashes <- PHIcrashes[PHIcrashes$MODEL_YR >= 2010, ]
View(PHIcrashes)

PHIcrashes <- PAcrashes %>%
  filter(DEC_LAT >= latitude_min, DEC_LAT <= latitude_max,
         DEC_LAT >= longitude_min, DEC_LONG <= longitude_max)

PHIcrashes$PARTIAL_VIN <- paste0(PHIcrashes$PARTIAL_VIN, "A0001")


PHIcrashes <- PHIcrashes[PHIcrashes$MAKE_CD %in% c("FORD", "TOYT", "HOND", "CHEV", "NISS"), ]

unique(PHIcrashes$PARTIAL_VIN)

PHIcrashes <- na.omit(PHIcrashes)
PHIcrashes <- PHIcrashes[rowSums(PHIcrashes == "") == 0, ]




qplot(DEC_LONG, # x (horizontal)
      DEC_LAT, # y (vertical)
      data=PHIcrashes, # data source
      geom="point") # plot type


#MANUFACTURERS INVOLVED IN CRASH
manufacturer_counts <- table(PHIcrashes$MAKE_CD)
manufacturer_counts <- sort(manufacturer_counts, decreasing = TRUE)
View(manufacturer_counts)

#COUNT OF REAR END COLLSIONS TO REST OF CAR
manufacturer_counts <- table(PHIcrashes$REAR_END)


PHIcrashes <- PHIcrashes[order(PHIcrashes$PARTIAL_VIN), ]

PHIcrashes$PARTIAL_VIN[PHIcrashes$PARTIAL_VIN %in% c('19UDE', '19VDE')] <- "ILX"
PHIcrashes$PARTIAL_VIN[PHIcrashes$PARTIAL_VIN %in% c('19UUA')] <- "TL"
PHIcrashes$PARTIAL_VIN[PHIcrashes$PARTIAL_VIN %in% c('19UUB')] <- "TLX"

phi_manufacturers <- as.data.frame(manufacturer_counts) 
View(phi_manufacturers)

write.csv(phi_manufacturers, file = "phi_manufacturers.csv", row.names = TRUE)
