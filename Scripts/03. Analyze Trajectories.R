#Title #####################################################
# T.VULTURE MOVE ANALYSIS. Script 03: TRAJECTORY ANALYSIS ##
#Function ##################################################
## Performs geometric analysis on the movement tracks
## 

# `Move2` (New) Workflow ----------------------------------------------------------------------------------
## Packages

library(move2)
library(move)
library(adehabitatLT)
library(sf)
library(lubridate)
library(rnaturalearth)
library(tidyverse)

## 0. Load the cleaned data -------------------------------------------------------------------------------
load(choose.files()) ## Select the file from the Cleaned Data folder

## 1. Net Square Displacement (NSD) Analysis
### Calculate NSD and set units
TVult1NSD <- (st_distance(TVult1.lc,TVult1.lc[1,]))^2
TVult1NSD <- units::set_units(TVult1NSD, "km^2")

### Plot NSD over time 
plot(mt_time(TVult1.lc), TVult1NSD, type="l",
     xlab="Time", ylab="Net square distance (Km²)")
#### The plot can help detect the moments in time when the bird stayed away from its usual nesting place.
#### It can also signal changes in nest selection and periods of low or high activity.

## 2. dBBMM Variance Analysis -----------------------------------------------------------------------------
### IN CONSTRUCTION...

## ?. Analysis of Azimuth, Speed and Turning Angles -------------------------------------------------------
### Categorize months based on rainfall seasonal regimes
TVult1.lc  <- mutate(TVult1.lc, month = month(mt_time(TVult1.lc)), 
                     season = recode_factor(month, "1"="Dry", "2"="Dry",
                                            "3"="Dry", "4"="Dry",
                                            "5"="Wet", "6"="Wet",
                                            "7"="Wet", "8"="Wet",
                                            "9"="Wet", "10"="Wet",
                                            "11"="Dry", "12"="Dry"))

### Add the information related to speed, azimuth and turning angle to the trajectory
TVult1.lc <- TVult1.lc %>% mutate(azimuth = mt_azimuth(TVult1.lc), ## Takes values between -180º and 180º, Positive is East cardinal direction
                                  speed = mt_speed(TVult1.lc), 
                                  turnangle = mt_turnangle(TVult1.lc)) ## Takes values between -180º and 180º, Positive is Turn towards the Right side