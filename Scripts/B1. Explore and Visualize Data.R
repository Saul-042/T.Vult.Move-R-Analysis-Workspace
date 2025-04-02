#Title ################################################
# T.VULTURE MOVE ANALYSIS. Script B1: VISUALIZE DATA ##
#Function #############################################
## Checks the spatio-temporal organization of the movement data
## Creates maps to chart the data in a geographical context 

# `move2` Workflow ----------------------------------------------------------------------------------------
## Packages

library(move2)
library(sf)
library(ggplot2)
library(units)
library(dplyr)

## 0. Load the clean data ---------------------------------------------------------------------------------
load(choose.files()) ## Select the file from the Data_Cleaned folder

## 1. Check the geographical position distribution of the data --------------------------------------------
### Option 1: use 'rworldxtra' package (DOESN'T NEED internet connection)
library(rworldxtra)
data("countriesHigh")
world <- st_as_sf(countriesHigh)
bbox <- st_bbox(TVult.mv2_loc)
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = mt_track_lines(TVult.mv2_loc), 
          aes(color = `individual-local-identifier`)) +
  coord_sf(xlim = c(bbox[1]-2, bbox[3]+2), 
           ylim = c(bbox[2]-2, bbox[4]+2), 
           expand = F)
  theme_void()

### Option 2: use 'ggspatial' package (NEEDS internet connection)
library(ggspatial)
ggplot() +
  annotation_map_tile(zoom = 9) +
  annotation_scale(aes(location = "br")) +
  theme_linedraw() +
  geom_sf(data = TVult.mv2_loc, 
          color = "darkgrey", 
          size = 1) +
  geom_sf(data = mt_track_lines(TVult.mv2_loc), 
          aes(color = `individual-local-identifier`)) +
  guides(color = "none")

### Option 3: use 'mapview' package (NEEDS an internet connection and a VPN (from .cu))
library(mapview)
#### Visualize the movement data as segments
mapView(mt_track_lines(TVult.mv2_loc), 
        zcol="individual-local-identifier", 
        legend=F)
#### Visualize the location records as points # <WARNING>: This is a high-intensity process for the computer.
mapView(TVult.mv2_loc, zcol="individual-local-identifier", legend=F)
##### Special case: Check the geographical position distribution of all the individuals
mapView(mt_track_lines(Cln_Vult.All$`individual-local-identifier`), 
        zcol="individual-local-identifier", 
        legend=F)

### Option 4: use 'ggmap' package (NEEDS an internet connection and a VPN, also NEEDS an API (check after feb.15.2025 to see if the API still works and go to the page to see if a new one can be obtained for free))
library(ggmap)
register_stadiamaps("e0acaa53-a221-4e6a-b475-7ea3e1177c2e", write = FALSE) # Register API key

map <- get_stadiamap(bbox = c(left = -81.54563, 
                              bottom = 21.00317, 
                              right = -77.26951, 
                              top = 23.08256),
                     zoom = 9, 
                     maptype = "stamen_terrain")
ggmap(map) +
  geom_sf(data = TVult.mv2_loc, inherit.aes = FALSE) +
  geom_sf(data = mt_track_lines(TVult.mv2_loc), 
          aes(color = `individual-local-identifier`), 
          inherit.aes = FALSE) +
  guides(color = "none")

## Option 4 > use the tmap package ## Check the package later
library(tmap)
# with a movestack just to inspect data
tmap_mode("view")
tm_shape(bats)+tm_dots()

# transform to e.g. sf class for more options
library(sf)
bats_SFp <- bats_df%>%st_as_sf(coords = c("location.long", "location.lat"), crs = crs(bats))%>%st_cast("POINT")
tmap_mode("view")
tm_shape(bats_SFp)+tm_dots(col = "individual.local.identifier")

## Save the movement lines as a '.shp' file for working on a GIS
st_write(mt_track_lines(TVult.mv2_loc), dsn = ".", 
             layer = "Data_Processed/AllTracks1", driver="ESRI Shapefile")

## Animate the tracks (doesn't work currently, wait till Windows can be activated to check if works)
data_interpolated <- TVult.mv2_loc[!sf::st_is_empty(TVult.mv2_loc), ] |>
  mt_interpolate(
    seq(
      as.POSIXct("2021-01-13"),
      as.POSIXct("2022-01-13"), "1 day" 
    ),
    max_time_lag = units::as_units(3, "day"),
    omit = TRUE
  )
animation <- ggplot() +
  annotation_map_tile(zoom = 9, progress = "none") +
  annotation_scale() +
  theme_linedraw() +
  geom_sf(
    data = data_interpolated, size = 3,
    aes(color = `individual-local-identifier`)
  ) +
  gganimate::transition_manual(timestamp) +
  labs(
    title = "Vulture CUBA-7198",
    subtitle = "Time: {current_frame}",
    color = "Individual"
  )
gganimate::animate(animation,
                   nframes = length(unique(data_interpolated$timestamp)))

## 2. Explore the temporal distribution and resolution of the data ----------------------------------------
### Tabulate the amount of records per year and month
table(year(mt_time(TVult.mv2_loc)), month(mt_time(TVult.mv2_loc)))

### Extract the time lags between records
timeLags <- mt_time_lags(TVult.mv2_loc) ## It uses by default the most convenient time unit
#### REMEMBER that the last value of the time lag vector will be an NA.

### Check the distribution of time lags between records
summary(timeLags)
timeLags_noU <- units::drop_units(timeLags) #Function hist() is not compatible with time "units", so they are dropped.

### Visualize the time lags
hist(timeLags_noU, breaks=50, main=NA, xlab="Time lag in seconds")
### Visualize the time lags shorter than 30 seconds
hist(timeLags_noU[timeLags_noU<30], breaks=25, main=NA, xlab="Time lag in seconds")

### Extract the time stamps of the movement data
ts <- mt_time(TVult.mv2_loc)
### Convert the time stamps into the local time of the study
tsLocal <- lubridate::with_tz(ts, tzone="America/Havana")
### Tabulate the number of locations per months and time of the day
TVult.mv2_loc %>% group_by(Month = lubridate::month(tsLocal), Hour = lubridate::hour(tsLocal)) %>% 
  summarize(N.locations = n()) %>% sf::st_drop_geometry() %>% print(n=200)

## 3. Explore the spatial distribution and resolution of the data -----------------------------------------
### Extract the lengths of segments of the movement data
dist <- set_units(mt_distance(TVult.mv2_loc), m)
### Visualize the distribution of distances between records
summary(dist)
hist(drop_units(dist), xlim = c(0,100), breaks=20000, main=NA)

### Extract the speed of movement between locations 
speeds <- set_units(mt_speed(TVult.mv2_loc), m/s)
### Visualize the distribution of velocity of movement between locations 
summary(speeds)
hist(drop_units(speeds), breaks="FD")

### Visualize the relation between time lags and speed of the segments
speedVsTimeLag <- data.frame(timeLag = timeLags, speeds = speeds)
speedVsTimeLag <- speedVsTimeLag[speedVsTimeLag$timeLag < set_units(10, hour) & speedVsTimeLag$speeds < set_units(20, m/s),]
plot(speedVsTimeLag$timeLag, speedVsTimeLag$speeds, xlab='Time lag', ylab='Speed', pch=19)

### Extract the azimuth of movement of the segments
direction <- mt_azimuth(TVult.mv2_loc)
### Visualize the distribution of the directions of movement of the segments
summary(direction)
hist(direction,  breaks = 18, xlab="Direction of movement", main = NA)

### Extract the turning angles between the segments
turnAngles <- mt_turnangle(TVult.mv2_loc)
### Visualize the distribution of the turning angles between the segments
summary(turnAngles)
hist(turnAngles, breaks = 18, xlab="Turning Angle", main = NA)
# Normally, the distribution should look like a bell curve, with most of the cases going close to 0º.

### Check for missed fixes in the movement data
unique(sf::st_is_empty(TVult.mv2_loc))


# `move` Workflow -----------------------------------------------------------------------------------------
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

(0.87*0.01)/(0.87*0.01+0.095*0.99)

