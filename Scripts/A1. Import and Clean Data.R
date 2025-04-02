#Title #######################################################
# T.VULTURE MOVE ANALYSIS. Script A1: IMPORT AND CLEAN DATA ##
#Function ####################################################
## Converts the downloaded .csv file from Movebank to a Move object.
## Cleans raw movement data and eliminate duplicates and incomplete records, 
### as well as multi-locations.
## IMPORTANT: This Script deletes all non-location sensor data, DO NOT DELETE the original .csv file. 


# `Move2` Workflow -------------------------------------------------------------------------------------
## Packages

library(move2)
library(sf)
library(dplyr)

## 1. Import the data ----------------------------------------------------------------------------------
## The data should have been downloaded from the Movebank repository in '.csv' format

### Directly search and choose the '.csv' files in the directory
TVult.mv2 <- mt_read(choose.files())
### Check the structure of the dataset
str(TVult.mv2)

## 2. Order and clean the data -------------------------------------------------------------------------

### Rearrange the recorded events by chronological order
TVult.mv2 <- arrange(TVult.mv2, mt_time(TVult.mv2))

### Check for any outliers in the dataset
mt_movebank_visible(TVult.mv2) %>% table() # The printed return should be TRUE in all cases
### If the return is FALSE, remove the outliers
TVult.mv2 <- mt_filter_movebank_visible(TVult.mv2)

### Check for missing values (i.e. events with no location data and non-positional sensor data)
st_is_empty(TVult.mv2) %>% table()
### Delete all observations with missing values
TVult.mv2_loc <- filter(TVult.mv2, !st_is_empty(TVult.mv2))

### Check for duplicated values
table(duplicated(mt_time(TVult.mv2_loc)))
### Exclude  all duplicates by selecting specific columns
TVult.mv2_loc <- TVult.mv2_loc[mt_unique(dplyr::select(TVult.mv2_loc, -c("event-id",
                                                                         "eobs:battery-voltage",
                                                                         "eobs:key-bin-checksum",
                                                                         "import-marked-outlier"))), ]
### Check for duplicated values again
table(duplicated(mt_time(TVult.mv2_loc)))


## 3. Save the result in the directory -----------------------------------------------------------------

### Save the Move2 object
#### IMPORTANT: CHANGE the file name of each Move2 Object to be consistent with the animal ID
save(TVult.mv2_loc, file = "Data_Cleaned/Mv2.ClnLocs.VultXXXX.Rdata")




# `Move` Workflow --------------------------------------------------------------------------------
## Packages 

library(move)

## 1. Import the data ----------------------------------------------------------------------------------
## The data has been downloaded from the Movebank repository in '.csv' format

### Directly search and choose the '.csv' files in the directory
TVult1 <- read.csv(choose.files())
### Check the structure of the dataset
str(TVult1)

#### Put together split datasets -----------------------------------------------------------------------
##### This section is built for datasets which are too large to download in one single file
###### PROTOCOL: Download the data from Movebank in chunks representing single natural years (e.g. 2021, 2022, ...)
###### This code imports each year individually and stacks them up in a single dataframe
### Choose the second year of data (likely 2022)
TVult1.2 <- read.csv(choose.files())
### Choose the third year of data (likely 2023)
TVult1.3 <- read.csv(choose.files())
### Merge all the imported datasets, use one option depending of number of parts the dataset was divided by
TVult1 <- rbind(TVult1, TVult1.2) ## OR ##
TVult1 <- rbind(TVult1, TVult1.2, TVult1.3)
### Delete non-necessary additional datasets from the Environment to save memory
rm(TVult1.2) ## OR ##
rm(TVult1.2, TVult1.3)


## 2. Pre-process the data -----------------------------------------------------------------------------

### Check and Correct Time stamps

#### Check the starting date
min(TVult1$timestamp)
#### Check the final date
max(TVult1$timestamp)

### Convert the date to POSIXct format
TVult1$timestamp <- as.POSIXct(TVult1$timestamp, format = "%F %T", tz = "UTC")
#### Order the time stamps in chronological order
TVult1 <- TVult1[order(TVult1$timestamp),]

### Eliminate entries with missing values and duplicates

#### Store the index of all observations with missing values of latitude, longitude or time stamp
index.complt <- complete.cases(TVult1[,c("location.lat", "location.long", "timestamp")])
#### Delete all observations with missing values
TVult1 <- TVult1[index.complt == TRUE,]

#### Check if there is any duplicated time stamps
any(duplicated(TVult1[c("timestamp")]))

##### Build an overview of the duplicates and identify the first one ###
dup <- getDuplicatedTimestamps(TVult1)
table(unlist(lapply(dup,function(x)length(x))))
dup[1]
TVult1[dup[[1]],]

#### Eliminate all exact duplicated entries (that share all data except their event-specific ID)
TVult1 <- TVult1[!duplicated(TVult1[,!names(TVult1) %in% "event.id"]),]

#### Check the locations of the first non-exact duplicate's (i.e. multi-locations) time stamp 
##### plus a couple of entries before and after 
TVult1[(anyDuplicated(TVult1[,"timestamp"])-2):(anyDuplicated(TVult1[,"timestamp"])+2),
      c("timestamp", "location.long", "location.lat", "study.local.timestamp")]

#### Remove all the multi-locations by choosing the entry that minimize the distance between 
##### neighboring locations
while(length(dup <- getDuplicatedTimestamps(TVult1))>0){
  allrowsTOremove <- lapply(1:length(dup), function(x){
    ## row numbers of duplicates
    rown <- dup[[x]]
    ## create a row number ID to find the duplicated time stamps in the subset per individual
    TVult1$rowNumber <- 1:nrow(TVult1)
    ## if the duplicated positions are in the middle of the table
    if(TVult1$rowNumber[1]<rown[1] & TVult1$rowNumber[nrow(TVult1)]>max(rown)){
      ### calculate total distance between fix before/after and the first alternate location
      dist1 <- sum(distHaversine(TVult1[TVult1$rowNumber%in%
                                          c((rown[1]-1),(max(rown)+1)),
                                          c("location.long", "location.lat")],
                                 TVult1[TVult1$rowNumber==rown[1],
                                          c("location.long", "location.lat")]))
      ### calculate total distance between fix before/after and the second alternate location
      dist2 <- sum(distHaversine(TVult1[TVult1$rowNumber%in%c((rown[1]-1),(max(rown)+1)),
                                          c("location.long", "location.lat")],
                                 TVult1[TVult1$rowNumber==rown[2],
                                          c("location.long", "location.lat")]))
      ### omit the alternate location that produces the longer route
      if(dist1<dist2){rowsTOremove <- rown[2]}else{rowsTOremove <- rown[1]}
    }
    ## in case the duplicated time stamps are the very first location of the dataset, calculate distance between 
    ### duplicate and following location
    if(TVult1$rowNumber[1]==rown[1]){
      dist1 <- sum(distHaversine(TVult1[TVult1$rowNumber==(max(rown)+1),
                                        c("location.long", "location.lat")],
                                 TVult1[TVult1$rowNumber==rown[1],
                                        c("location.long", "location.lat")]))
      dist2 <- sum(distHaversine(TVult1[TVult1$rowNumber==(max(rown)+1),
                                        c("location.long", "location.lat")],
                                 TVult1[TVult1$rowNumber==rown[2],
                                        c("location.long", "location.lat")]))
      ### and omit the alternate location that produces the longer route
      if(dist1<dist2){rowsTOremove <- rown[2]}else{rowsTOremove <- rown[1]}
    }
    ## in case the duplicated time stamps are the very last positions, calculate distance between 
    ### duplicate and previous location
    if(TVult1$rowNumber[nrow(TVult1)]==max(rown)){
      dist1 <- sum(distHaversine(TVult1[TVult1$rowNumber==(rown[1]-1),
                                        c("location.long", "location.lat")],
                                 TVult1[TVult1$rowNumber==rown[1],
                                        c("location.long", "location.lat")]))
      dist2 <- sum(distHaversine(TVult1[TVult1$rowNumber==(rown[1]-1),
                                        c("location.long", "location.lat")],
                                 TVult1[TVult1$rowNumber==rown[2],
                                        c("location.long", "location.lat")]))
      ### and omit the alternate location that produces the longer route
      if(dist1<dist2){rowsTOremove <- rown[2]}else{rowsTOremove <- rown[1]}
    }
    return(rowsTOremove)
  })
  TVult1 <- TVult1[-unique(sort(unlist(allrowsTOremove))),]
  TVult1$rowNumber <- NULL
}

## 3. Make a Move object and save it in the directory --------------------------------------------------

### Convert the dataset to the Move class
TVult.mv1 <- move(x = TVult1$location.long, y = TVult1$location.lat, time = TVult1$timestamp,
                 data = TVult1, proj = crs("+proj=longlat +datum=WGS84"),
                 animal = TVult1$individual.local.identifier, sensor = TVult1$sensor.type)

### Save the resulting Move object
#### IMPORTANT: CHANGE the file name of each Move Object to be consistent with the animal ID
save(TVult.mv1, file = "Data_Cleaned/Mv1.Cln.Vult7xxxdata.Rdata")
