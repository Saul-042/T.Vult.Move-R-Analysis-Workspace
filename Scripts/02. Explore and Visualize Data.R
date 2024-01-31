#Title################################################
# T.VULTURE MOVE ANALYSIS. Script 02: VISUALIZE DATA #
#Function#############################################
## Checks the spatio-temporal organization of the movement data
## Creates maps to pre-visualize the extent of the data in a geographical context 

# `Move2` (New) Workflow ----------------------------------------------------------------------------------



# `Move` (Old) Workflow -----------------------------------------------------------------------------------
## Packages -----------------------------------------------------------------------------------------------
library(move)
library(mapview)
library(lubridate)

## 0. Load the cleaned data -------------------------------------------------------------------------------
load(choose.files()) ## Select the file from the Cleaned Data folder

## 1. Check the general spatial distribution of the data --------------------------------------------------
### Creates a map and superimposes the spatial locations. 
#### <IMPORTANT>: This is a high intensity process for the computer.
mapview(TVult.mv1)

### Visualize the distance between recorded locations
summary(distance(TVult.mv1)) # Its useful to check the extremes of the data distance distribution to check for outliers.
hist(distance(TVult.mv1), main=NA, breaks = 50000, xlim = c(0,50))

### Visualize the speed between recorded locations
summary(speed(TVult.mv1)) # Its useful to check the extremes of the data speed distribution to check for outliers.
hist(speed(TVult.mv1), breaks="FD", xlim = c(0,30))

### Visualize the heading of the movement
hist(angle(TVult.mv1),  breaks=18, xlab="Direction of movement", main=NA)
# The histogram is supposed to not show a bias in any direction.

### Visualize the turning angles between consecutive steps
hist(turnAngleGc(TVult.mv1), breaks=18, xlab="Turning Angle", main=NA)
# Normally, it should look like a bell curve.


## 2. Check the temporal resolution of the data -----------------------------------------------------------
### Save the time spans between each recorded location
timelags <- timeLag(TVult.mv1, units='secs')
timelags
### Visualize the time lags
hist(timelags, breaks=10, main=NA, xlab="Time lag in seconds") 
### Visualize the time lags shorter than 30 seconds
hist(timelags[timelags<30], breaks=10, main=NA, xlab="Time lag in seconds")

### Save the time of the day of each location as a POSIXct timestamps
tmstamp <- timestamps(TVult.mv1)
### Transform the timestamps into local time
tmstampLocal <- with_tz(tmstamp, tzone="America/Havana")
### List with the distribution of records across hours of the day
tapply(tmstampLocal, hour(tmstampLocal), length)
### Table with the distribution of records across hours of the day and months of the year
tapply(tmstampLocal, list(month(tmstampLocal),hour(tmstampLocal)), length)
