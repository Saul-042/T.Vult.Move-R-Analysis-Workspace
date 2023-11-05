#Title############################################
# T.VULTURE MOVE ANALYSIS. Script 01: CLEAN DATA #
#Function#########################################
## Converts the downloaded .csv file from Movebank to a Move object
## Cleans raw movement data and eliminate duplicates and incomplete records, as well as multi-locations

# `Move` (Old) Workflow -----------------------------------------------------------------------------
## Packages -----------------------------------------------------------------------------------------
library(move)
library(tidyverse)


## 1. Import the data -------------------------------------------------------------------------------
## The data has been downloaded from the Movebank repository in '.csv' format

### Directly search and choose the '.csv' files in the directory
TVult <- read.csv(choose.files())
### Check the structure of the dataset
str(TVult)

#### Put together split datasets -----------------------------------------
##### This section is built for datasets which are too large to download in one single file
###### PROTOCOL: Download the data from Movebank in chunks representing single natural years (e.g. 2021, 2022, ...)
###### This code imports each year individually and stacks them up in a single dataframe
### Choose the second year of data (likely 2022)
TVult.2 <- read.csv(choose.files())
### Choose the third year of data (likely 2023)
TVult.3 <- read.csv(choose.files())
### Merge all the imported datasets, use one option depending of number of parts the dataset was divided by
TVult <- rbind(TVult, TVult.2) ## OR ##
TVult <- rbind(TVult, TVult.2, TVult.3)
### Delete non-necessary additional datasets from the Environment to save memory
rm(TVult.2) ## OR ##
rm(TVult.2, TVult.3)


## 2. Pre-process the data ----------------------------------------------------------------------------

### Check and Correct Time stamps

#### Check the starting date
min(TVult$timestamp)
#### Check the final date
max(TVult$timestamp)

#### Convert the date to POSIXct format
TVult$timestamp <- as.POSIXct(TVult$timestamp, format = "%F %T", tz = "UTC")
#### Order the time stamps in chronological order
TVult <- TVult[order(TVult$timestamp),]

### Eliminate entries with missing values and duplicates

#### Store the index of all observations with missing values of latitude, longitude or time stamp
index.complt <- complete.cases(TVult[,c("location.lat", "location.long", "timestamp")])
#### Delete all observations with missing values
TVult <- TVult[index.complt == TRUE,]

#### Check if there is any duplicated time stamps
any(duplicated(TVult[c("timestamp")]))

##### Build an overview of the duplicates and identify the first one ###
dup <- getDuplicatedTimestamps(TVult)
table(unlist(lapply(dup,function(x)length(x))))
dup[1]
TVult[dup[[1]],]

#### Eliminate all exact duplicated entries (that share all data except their event-specific ID)
TVult <- TVult[!duplicated(TVult[,!names(TVult) %in% "event.id"]),]

#### Identify and store first non-exact duplicate (i.e. multi-locations)
TVult.dup <- anyDuplicated(TVult[,"timestamp"])

#### Check the locations of the identified multi-location's time stamp plus a couple of entries before and after 
TVult[(TVult.dup-2):(TVult.dup+2),c("timestamp", "location.long", "location.lat", 
                                    "study.local.timestamp")]

#### Remove all the multi-locations by choosing the entry that minimize the distance between neighboring locations
while(length(dup <- getDuplicatedTimestamps(TVult))>0){
  allrowsTOremove <- lapply(1:length(dup), function(x){
    ## row numbers of duplicates
    rown <- dup[[x]]
    ## create a row number ID to find the duplicated time stamps in the subset per individual
    TVult$rowNumber <- 1:nrow(TVult)
    ## if the duplicated positions are in the middle of the table
    if(TVult$rowNumber[1]<rown[1] & TVult$rowNumber[nrow(TVult)]>max(rown)){
      ### calculate total distance between fix before/after and the first alternate location
      dist1 <- sum(distHaversine(TVult[TVult$rowNumber%in%c((rown[1]-1),(max(rown)+1)),c("location.long", "location.lat")],
                                 TVult[TVult$rowNumber==rown[1],c("location.long", "location.lat")]))
      ### calculate total distance between fix before/after and the second alternate location
      dist2 <- sum(distHaversine(TVult[TVult$rowNumber%in%c((rown[1]-1),(max(rown)+1)),c("location.long", "location.lat")],
                                 TVult[TVult$rowNumber==rown[2],c("location.long", "location.lat")]))
      ### omit the alternate location that produces the longer route
      if(dist1<dist2){rowsTOremove <- rown[2]}else{rowsTOremove <- rown[1]}
    }
    ## in case the duplicated time stamps are the very first location of the dataset, calculate distance between duplicate and following location
    if(TVult$rowNumber[1]==rown[1]){
      dist1 <- sum(distHaversine(TVult[TVult$rowNumber==(max(rown)+1),c("location.long", "location.lat")],
                                 TVult[TVult$rowNumber==rown[1],c("location.long", "location.lat")]))
      dist2 <- sum(distHaversine(TVult[TVult$rowNumber==(max(rown)+1),c("location.long", "location.lat")],
                                 TVult[TVult$rowNumber==rown[2],c("location.long", "location.lat")]))
      ### and omit the alternate location that produces the longer route
      if(dist1<dist2){rowsTOremove <- rown[2]}else{rowsTOremove <- rown[1]}
    }
    ## in case the duplicated time stamps are the very last positions, calculate distance between duplicate and previous location
    if(TVult$rowNumber[nrow(TVult)]==max(rown)){
      dist1 <- sum(distHaversine(TVult[TVult$rowNumber==(rown[1]-1),c("location.long", "location.lat")],
                                 TVult[TVult$rowNumber==rown[1],c("location.long", "location.lat")]))
      dist2 <- sum(distHaversine(TVult[TVult$rowNumber==(rown[1]-1),c("location.long", "location.lat")],
                                 TVult[TVult$rowNumber==rown[2],c("location.long", "location.lat")]))
      ### and omit the alternate location that produces the longer route
      if(dist1<dist2){rowsTOremove <- rown[2]}else{rowsTOremove <- rown[1]}
    }
    return(rowsTOremove)
  })
  TVult <- TVult[-unique(sort(unlist(allrowsTOremove))),]
  TVult$rowNumber <- NULL
}

## 3. Make a Move object and save it in the directory -------------------------------

### Convert the dataset to the Move class
TVult.move <- move(x = TVult$location.long, y = TVult$location.lat, time = TVult$timestamp,
                   data = TVult, proj = crs("+proj=longlat +datum=WGS84"),
                   animal = TVult$individual.local.identifier, sensor = TVult$sensor.type)

### Save the resulting Move object
#### IMPORTANT: CHANGE the file name of each Move Object to be consistent with the animal ID
save(TVult.move, file = "Data_Processed/Cln.Vulture7xxx.Rdata")


# `Move2` (New) Workflow ----------------------------------------------------------------------------
## Packages

library(move2)

## 1. Import the data -------------------------------------------------------------------------------
## The data has been downloaded from the Movebank repository in '.csv' format

### Directly search and choose the '.csv' files in the directory
TVult1 <- read.csv(choose.files())
### Check the structure of the dataset
str(TVult1)
