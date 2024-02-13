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

### Categorize months based on rainfall seasonsal regimes
TVult1.lc  <- mutate(TVult1.lc, month = month(mt_time(TVult1.lc)), 
                                season = recode_factor(month, "1"="Dry", "2"="Dry",
                                                              "3"="Dry", "4"="Dry",
                                                              "5"="Wet", "6"="Wet",
                                                              "7"="Wet", "8"="Wet",
                                                              "9"="Wet", "10"="Wet",
                                                              "11"="Dry", "12"="Dry"))
                       
## 1. Analysis of Azimuth, Speed and Turning Angles -------------------------------------------------------
### Add the information related to speed, azimuth and turning angle to the trajectory
TVult1.lc <- TVult1.lc %>% mutate(azimuth = mt_azimuth(TVult1.lc), ## Takes values between -180º and 180º, Positive is East cardinal direction
                      speed = mt_speed(TVult1.lc), 
                      turnangle = mt_turnangle(TVult1.lc)) ## Takes values between -180º and 180º, Positive is Turn towards the Right side

