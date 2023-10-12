# T.VULT Script-01: DATA CLEAN
## Cleans raw movement data and converts the output .csv file from Movebank to Move format
## (Eliminates duplicates and incomplete records, as well as multi-locations)

## Packages: ----
library(move)
library(tidyverse)

## IMPORTING DATA ---------------------------------------------------------------------------
#TVult.df1 <- read.csv("~/Data_Raw/7198/Cuba-7198.csv") ### Old import code
TVult.df1 <- read.csv(choose.files()) ## This allows to directly choose the .csv file

### PUTING TOGETHER SPLIT DATASETS ----------------------------
#This section is built due to several datasets being too large to download in one single file
#The protocol is to download the data from Movebank in chunks representing single natural years (e.g. 2021, 2022, ...)
#This code imports each year individually and stacks them up on a single dataframe 
TVult.df2 <- read.csv(choose.files()) ## Choose the second year of data (likely 2022) 
TVult.df3 <- read.csv(choose.files()) ## Choose the third year of data (likely 2023)

#### Merge all the imported datasets, use either option depending of number of parts the dataset was divided by
TVult.df1 <- rbind(TVult.df1, TVult.df2)
TVult.df1 <- rbind(TVult.df1, TVult.df2, TVult.df3)
# Deletes non-necessary additional data from the Global Environment to save Memory
rm(TVult.df2)
rm(TVult.df2, TVult.df3)

## TO DO TESTS TO SCRIPTS WITH A REDUCED DATASET ----------------------------
### This deletes the entries after a fixed date from the dataset. 
### THIS IS FOR TEST PURPOSE: IGNORE IN REAL ANALYSES
#TVult.df1 <- TVult.df1 %>% filter(timestamp <= "2021-06-01 00:00:00.000")


## PREPARING DATA ---------------------------------------------------------------------------
# Checking Timestamps ----
min(TVult.df1$timestamp) #Check the starting date
max(TVult.df1$timestamp) #Check the final date

# Convert the date to POSIXct format ----
TVult.df1$timestamp <- as.POSIXct(TVult.df1$timestamp, format="%F %T", tz="UTC")


# PROCESSING DATA ---------------------------------------------------------------------------
## 01 -> Delete all observations with missing lat, long or timestamp values ----
indx.cmplt <- complete.cases(TVult.df1[c("location.lat", "location.long", "timestamp"),])
TVult.df1.complt <- TVult.df1[indx.cmplt == TRUE,]

## 02 -> Check if there is any duplicated timestamps ----
any(duplicated(TVult.df1.complt[c("timestamp","individual.local.identifier")]))

## 03 -> Eliminate duplicated observations (both exact or incomplete) by comparing location and timestamps ----
TVult.df1.complt <- TVult.df1.complt[!duplicated(TVult.df1.complt[c("timestamp", 
                                                                    "location.long", 
                                                                    "location.lat", 
                                                                    "individual.local.identifier")]),]

#Step 3.1 -> Ensure that timestamps and individuals are ordered ----
### This will make sure the previous record is the same when checking for duplicates
TVult.df1.complt <- TVult.df1.complt[order(TVult.df1.complt$individual.local.identifier,
                                           TVult.df1.complt$timestamp),]
#Step 3.1.1 -> Identify and store first duplicate
TVult.df1.dupl <- anyDuplicated(TVult.df1.complt[,c("timestamp","individual.local.identifier")])

#Step 3.1.2 -> Identify the issue by looking both at a bit before and a bit after the incidence
TVult.df1.complt[(TVult.df1.dupl-2):(TVult.df1.dupl+1),c("timestamp", "location.long",
                                                         "location.lat", "study.local.timestamp")]

#Step 3.1.3 -> A "while" loop will ensure that duplicates identification is done until each multi-location is removed
#In each duplicate, the location that minimize the total distance covered is choosen
while(TVult.df1.dupl <- anyDuplicated(TVult.df1.complt[,c("timestamp","individual.local.identifier")]))
{
  print("ok")
  # calculate total distance throught the first alternate location
  print(TVult.df1.complt[c(TVult.df1.dupl-2, TVult.df1.dupl+1), c("location.long", "location.lat")])
  print(TVult.df1.complt[c(TVult.df1.dupl-1), c("location.long", "location.lat")])
  dist1 <- sum(distHaversine(TVult.df1.complt[c(TVult.df1.dupl-2, TVult.df1.dupl+1),
                                              c("location.long", "location.lat")],
                             TVult.df1.complt[c(TVult.df1.dupl-1), 
                                              c("location.long", "location.lat")]))
  
  # calculate total distance throught the second alternate location
  print(class(dist1))
  dist2 <- sum(distHaversine(TVult.df1.complt[c(TVult.df1.dupl-2, TVult.df1.dupl+1),
                                              c("location.long", "location.lat")],
                             TVult.df1.complt[TVult.df1.dupl, c("location.long", "location.lat")]))
  # omit the alternate location that produces the longer route
  print("ok2")
  if(dist1 < dist2){
    TVult.df1.complt <- TVult.df1.complt[-TVult.df1.dupl,]
  }
  else{
    TVult.df1.complt <- TVult.df1.complt[-(TVult.df1.dupl-1),]
  }
}    

#Step 4 -> Define the data.frame as a "move" object after cleaning with local time ----
TVult.df1.complt$study.local.timestamp <- as.POSIXct(TVult.df1.complt$study.local.timestamp,
                                                     format="%F %T", tz="UTC")
Vulture.move <- move(x=TVult.df1.complt$location.long,
                     y=TVult.df1.complt$location.lat,
                     time=TVult.df1.complt$study.local.timestamp,
                     data=TVult.df1.complt,
                     proj=CRS("+proj=longlat +datum=WGS84"),
                     animal=TVult.df1.complt$individual.local.identifier)

#Step 6 -> Save the resulting processed data ----
# save the move object, CHANGE THE NAME of the file to be consistent with the animal id
save(Vulture.move, 
     file = "D:/INBOX/-0-WORK/0-PhD Turkey Vultures/0-Research/0-Movebank Data/Cln.Vulture7616.Rdata")

# save as a text file, CHANGE THE NAME of the file to be consistent with the animal id
write.table(Vulture.move, 
            file = "D:/INBOX/-0-WORK/0-PhD Turkey Vultures/0-Research/0-Movebank Data/Clean.Vult7616.csv",
            sep = ",", row.names = FALSE)
