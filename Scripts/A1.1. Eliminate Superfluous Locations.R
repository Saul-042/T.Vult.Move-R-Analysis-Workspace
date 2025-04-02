#Title ######################################################################
# T.VULTURE MOVE ANALYSIS. Script A1.1: ELIMINATE SUPERFLUOUS? GPS LOCATIONS ##
#Function ###################################################################
## Deletes the last several locations in six Turkey Vultures, due to a lack of meaningful movement. 
## The tags continued transmitting after the animals stopped moving, according to the Movebank app.


# `Move2` (New) Workflow ----------------------------------------------------------------------------------
## Packages

library(move2)
library(dplyr)

## 1. Delete data from Cuba.7269 "Milagros" ---------------------------------------------------------------
##### The last locations from this animal show a sudden jump to a city, likely due to the e-obs Tag being recovered.
##### The last couple of days in this track will not be taken into account.

### Load file from the specific Vulture from the Data_Cleaned folder
load("Data_Cleaned/Mv2.ClnLocs.Vult7269.Rdata")

### Eliminate the not-useful data
TVult.mv2_loc <- TVult.mv2_loc %>% filter(`study-local-timestamp` < "2021-04-03 14:00:00")

### Save the file to the Clean_data folder
save(TVult.mv2_loc, file = "Data_Cleaned/Mv2.ClnLocs.Vult7269.woLastLocs.Rdata")


## 2. Delete data from Cuba.7577 "Elio" ------------------------------------------------------------------
##### This animal reached a location a stayed there for 7 days before the transmitter stopped.
##### The last week in this track will not be taken into account.

### Load file from the specific Vulture from the Data_Cleaned folder
load("Data_Cleaned/Mv2.ClnLocs.Vult7577.Rdata")

### Eliminate the not-useful data
TVult.mv2_loc <- TVult.mv2_loc %>% filter(`study-local-timestamp` < "2021-09-11")

### Save the file to the Clean_data folder
save(TVult.mv2_loc, file = "Data_Cleaned/Mv2.ClnLocs.Vult7577.woLastLocs.Rdata")


## 3. Delete data from Cuba.7587 "Cuco" ------------------------------------------------------------------
##### This animal probably was hunted and relayed during several months from the same location in a house in Havana before the transmitter stopped.
##### The last seven months in this track will not be taken into account.

### Load file from the specific Vulture from the Data_Cleaned folder
load("Data_Cleaned/Mv2.ClnLocs.Vult7587.Rdata")

### Eliminate the not-useful data
TVult.mv2_loc <- TVult.mv2_loc %>% filter(`study-local-timestamp` < "2021-11-27 16:00:00")

### Save the file to the Clean_data folder
save(TVult.mv2_loc, file = "Data_Cleaned/Mv2.ClnLocs.Vult7587.woLastLocs.Rdata")


## 4. Delete data from Cuba.7591 "Lourdes" ---------------------------------------------------------------
##### This animal reached a location in the south coast of Pinar del Rio and the transmitter stopped moving for a few weeks before it cut the data collecting.
##### The last two months in this track will not be taken into account.

### Load file from the specific Vulture from the Data_Cleaned folder
load("Data_Cleaned/Mv2.ClnLocs.Vult7591.Rdata")

### Eliminate the not-useful data
TVult.mv2_loc <- TVult.mv2_loc %>% filter(`study-local-timestamp` < "2021-06-06")

### Save the file to the Clean_data folder
save(TVult.mv2_loc, file = "Data_Cleaned/Mv2.ClnLocs.Vult7591.woLastLocs.Rdata")


## 5. Delete data from Cuba.7616 "Margit" ----------------------------------------------------------------
##### This animal reached its final location and the transmitter stopped moving.
##### The last two days in this track will not be taken into account.

### Load file from the specific Vulture from the Data_Cleaned folder
load("Data_Cleaned/Mv2.ClnLocs.Vult7616.Rdata")

### Eliminate the not-useful data
TVult.mv2_loc <- TVult.mv2_loc %>% filter(`study-local-timestamp` < "2024-01-26")

### Save the file to the Clean_data folder
save(TVult.mv2_loc, file = "Data_Cleaned/Mv2.ClnLocs.Vult7616.woLastLocs.Rdata")


## 6. Delete data from Cuba.7641 "Yudi" ------------------------------------------------------------------
##### This animal reached a location in the center of Mayabeque and the transmitter keep collecting data for several months before it stopped.
##### The last four months in this track will not be taken into account.

### Load file from the specific Vulture from the Data_Cleaned folder
load("Data_Cleaned/Mv2.ClnLocs.Vult7641.Rdata")

### Eliminate the not-useful data
TVult.mv2_loc <- TVult.mv2_loc %>% filter(`study-local-timestamp` < "2021-03-28")

### Save the file to the Clean_data folder
save(TVult.mv2_loc, file = "Data_Cleaned/Mv2.ClnLocs.Vult7641.woLastLocs.Rdata")