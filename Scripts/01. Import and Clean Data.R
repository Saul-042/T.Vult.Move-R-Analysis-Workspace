##################################################
# T.VULTURE MOVE ANALYSIS. Script 01: CLEAN DATA #
##################################################
## Cleans raw movement data and converts the output .csv file from Movebank-organized data to Move format
## It will eliminate duplicates and incomplete records, as well as multi-locations


# Packages -----------------------------------------------------------------------------------------
library(move)
library(tidyverse)


# 1. Import the data -------------------------------------------------------------------------------
# The data has already been downloaded from the Movebank repository in '.csv' format

## Directly search and choose the '.csv' file in the directory
TVult.df1 <- read.csv(choose.files())

### Put together split datasets -----------------------------------------
#### This section is built for datasets which are too large to download in one single file
##### PROTOCOL: Download the data from Movebank in chunks representing single natural years (e.g. 2021, 2022, ...)
##### This code imports each year individually and stacks them up in a single dataframe
## Choose the second year of data (likely 2022)
TVult.df2 <- read.csv(choose.files())
## Choose the third year of data (likely 2023)
TVult.df3 <- read.csv(choose.files())
## Merge all the imported datasets, use one option depending of number of parts the dataset was divided by
TVult.df1 <- rbind(TVult.df1, TVult.df2) ## OR ##
TVult.df1 <- rbind(TVult.df1, TVult.df2, TVult.df3)
## Delete non-necessary additional datasets from the Environment to save memory
rm(TVult.df2)
rm(TVult.df2, TVult.df3)


# 2. Preparing the data ----------------------------------------------------------------------------

## Checking Timestamps

### Check the starting date
min(TVult.df1$timestamp)
### Check the final date
max(TVult.df1$timestamp)

## Convert the date to POSIXct format
TVult.df1$timestamp <- as.POSIXct(TVult.df1$timestamp, format="%F %T", tz="UTC")

# 3. PROCESSING DATA ---------------------------------------------------------------------------
## Delete all observations with missing lat, long or timestamp values
indx.cmplt <- complete.cases(TVult.df1[c("location.lat", "location.long", "timestamp"),])
TVult.df1.complt <- TVult.df1[indx.cmplt == TRUE,]

## Check if there is any duplicated timestamps
any(duplicated(TVult.df1.complt[c("timestamp","individual.local.identifier")]))

## 03 -> Eliminate duplicated observations (both exact or incomplete) by comparing location and timestamps ----
TVult.df1.complt <- TVult.df1.complt[!duplicated(TVult.df1.complt[c("timestamp", 
                                                                    "location.long", 
                                                                    "location.lat", 
                                                                    "individual.local.identifier")]),]