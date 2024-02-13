#Title ################################################
# T.VULTURE MOVE ANALYSIS. Script 02: VISUALIZE DATA ##
#Function #############################################
## Checks the spatio-temporal organization of the movement data
## Creates maps to pre-visualize the extent of the data in a geographical context 

# `Move2` (New) Workflow ----------------------------------------------------------------------------------
## Packages

library(move2)
library(mapview)
library(dplyr)

## 0. Load the cleaned data -------------------------------------------------------------------------------
load(choose.files()) ## Select the file from the Cleaned Data folder

## 1. Check the general geographical distribution of the data ---------------------------------------------
### Change the class of the object to `sf`
VultSF <- TVult1.lc
class(VultSF) <- class(TVult1.lc) %>% setdiff("move2") # Remove the "move2" class

### Visualize the location records as points
#### <IMPORTANT WARNING>: This is a high intensity process for the computer.
mapview::mapView(VultSF, zcol="individual-local-identifier", legend=F)
### Visualize the movement data as segments
mapview::mapView(mt_track_lines(TVult1.lc), zcol="individual-local-identifier", legend=F)

## 2. Explore the temporal distribution and resolution of the data ----------------------------------------
### Tabulate the amount of records per year and month
table(year(mt_time(TVult1.lc)), month(mt_time(TVult1.lc)))

### Extract the time lags between records
timeLags <- mt_time_lags(TVult1.lc) ## It uses by default the most convenient time unit
#### REMEMBER that the last value of the time lag vector will be an NA.

### Check the distribution of time lags between records
summary(timeLags)
timeLags_noU <- units::drop_units(timeLags) #Function hist() is not compatible with time "units", so they are dropped.

### Visualize the time lags
hist(timeLags_noU, breaks=50, main=NA, xlab="Time lag in seconds")
### Visualize the time lags shorter than 30 seconds
hist(timeLags_noU[timeLags_noU<30], breaks=25, main=NA, xlab="Time lag in seconds")

### Extract the time stamps of the movement data
ts <- mt_time(TVult1.lc)
### Convert the time stamps into the local time of the study
tsLocal <- lubridate::with_tz(ts, tzone="America/Havana")
### Tabulate the number of locations per months and time of the day
TVult1.lc %>% group_by(Month = lubridate::month(tsLocal), Hour = lubridate::hour(tsLocal)) %>% 
  summarize(N.locations = n()) %>% sf::st_drop_geometry() %>% print(n=200)

## 3. Explore the spatial distribution and resolution of the data -----------------------------------------
### Extract the lengths of segments of the movement data
dist <- units::set_units(mt_distance(TVult1.lc), m)
### Visualize the distribution of distances between records
summary(dist)
hist(drop_units(dist), xlim = c(0,100), breaks=20000, main=NA)

### Extract the speed of movement between locations 
speeds <- units::set_units(mt_speed(TVult1.lc), m/s)
### Visualize the distribution of velocity of movement between locations 
summary(speeds)
hist(drop_units(speeds), breaks="FD")

### Visualize the relation between time lags and speed of the segments
speedVsTimeLag <- data.frame(timeLag = timeLags, speeds = speeds)
speedVsTimeLag <- speedVsTimeLag[speedVsTimeLag$timeLag < set_units(10, hour) & speedVsTimeLag$speeds < set_units(20, m/s),]
plot(speedVsTimeLag$timeLag, speedVsTimeLag$speeds, xlab='Time lag', ylab='Speed', pch=19)

### Extract the azimuth of movement of the segments
direction <- mt_azimuth(TVult1.lc)
### Visualize the distribution of the directions of movement of the segments
summary(direction)
hist(direction,  breaks = 18, xlab="Direction of movement", main = NA)

### Extract the turning angles between the segments
turnAngles <- mt_turnangle(TVult1.lc)
### Visualize the distribution of the turning angles between the segments
summary(turnAngles)
hist(turnAngles, breaks = 18, xlab="Turning Angle", main = NA)
# Normally, the distribution should look like a bell curve, with most of the cases going close to 0º.

### Check for missed fixes in the movement data
unique(sf::st_is_empty(TVult1.lc))


# `Move` (Old) Workflow -----------------------------------------------------------------------------------
## Packages 
library(move)
library(mapview)
library(lubridate)

## 0. Load the cleaned data -------------------------------------------------------------------------------
load(choose.files()) ## Select the file from the Cleaned Data folder

## 1. Check the general spatial distribution of the data --------------------------------------------------
### Creates a map and superimposes the recorded locations. 
#### <IMPORTANT WARNING>: This is a high intensity process for the computer.
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
# Normally, the distribution should look like a bell curve, with most of the cases going close to 0º.


## 2. Check the temporal resolution of the data -----------------------------------------------------------
### Save the time lags between each recorded location
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
