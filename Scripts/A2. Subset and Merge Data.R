#Title #######################################################
# T.VULTURE MOVE ANALYSIS. Script A2: SUBSET AND MERGE DATA ##
#Function ####################################################
## Join the movement data from separate files, in case they were downloaded from Movebank as different files. 
## Puts together the movement tracks of all the animals into a single whole 'move2' object.
## Lowers the resolution of the data to make it easier to work with.

# `Move2` Workflow -------------------------------------------------------------------------------------
## Packages

library(move2)
library(dplyr)

# Join different files ---------------------------------------------------------------------------------
### Load each file from all Vultures from the Data_Cleaned folder, either the resulting files from 
##### Script A1 and the files without Last Locations from Script A1.1
load("Data_Cleaned/Mv2.ClnLocs.Vult7593.Rdata")
Cln_Vult.7508 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7593_frJun24toMar25.Rdata")
Cln_Vult.7508_latest <- TVult.mv2_loc

### Delete duplicates in both files
Cln_Vult.7593 <- Cln_Vult.7508 %>% filter(`study-local-timestamp` < "2024-07-08")
Cln_Vult.7593_latest <- Cln_Vult.7508_latest %>% filter(`study-local-timestamp` >= "2024-07-08")

### Join both tracks into a single 'move2' object
TVult.mv2_loc <- rbind.data.frame(Cln_Vult.7593, Cln_Vult.7593_latest)
### Check duplipcates
table(duplicated(mt_time(TVult.mv2_loc)))

### Save resulting file
save(TVult.mv2_loc, file = "Data_Cleaned/Mv2.ClnLocs.Vult7593.Rdata")

# Merge all movement tracks into a single object -------------------------------------------------------
## 0. Load the cleaned data ----------------------------------------------------------------------------
### Load each file from all Vultures from the Data_Cleaned folder
### <INSTRUCTION: Run the whole chunk as a block>
load("Data_Cleaned/Mv2.ClnLocs.Vult7198.Rdata")
Cln_Vult.7198 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7269.woLastLocs.Rdata")
Cln_Vult.7269 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7346.Rdata")
Cln_Vult.7346 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7508.Rdata")
Cln_Vult.7508 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7572.Rdata")
Cln_Vult.7572 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7573.Rdata")
Cln_Vult.7573 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7574.Rdata")
Cln_Vult.7574 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7576.Rdata")
Cln_Vult.7576 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7577.woLastLocs.Rdata")
Cln_Vult.7577 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7578.Rdata")
Cln_Vult.7578 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7587.woLastLocs.Rdata")
Cln_Vult.7587 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7591.woLastLocs.Rdata")
Cln_Vult.7591 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7593.Rdata")
Cln_Vult.7593 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7615.Rdata")
Cln_Vult.7615 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7616.woLastLocs.Rdata")
Cln_Vult.7616 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7641.woLastLocs.Rdata")
Cln_Vult.7641 <- TVult.mv2_loc
load("Data_Cleaned/Mv2.ClnLocs.Vult7642.Rdata")
Cln_Vult.7642 <- TVult.mv2_loc
## Vulture Cuba-7662 is excluded from the analysis due to never been a free-ranging animal.
load("Data_Cleaned/Mv2.ClnLocs.Vult8108.Rdata")
Cln_Vult.8108 <- TVult.mv2_loc
rm(TVult.mv2_loc)

## 1. Merge the tracks ---------------------------------------------------------------------------------
### Join all the movement tracks into a single 'move2' object
Cln_Vult.All <- mt_stack(Cln_Vult.7198, Cln_Vult.7269, Cln_Vult.7346, Cln_Vult.7508, 
                         Cln_Vult.7572, Cln_Vult.7573, Cln_Vult.7574, Cln_Vult.7576, 
                         Cln_Vult.7577, Cln_Vult.7578, Cln_Vult.7587, Cln_Vult.7591, 
                         Cln_Vult.7593, Cln_Vult.7615, Cln_Vult.7616, Cln_Vult.7641, 
                         Cln_Vult.7642, Cln_Vult.8108)

### Remove the individual datasets for each movement track
rm(Cln_Vult.7198, Cln_Vult.7269, Cln_Vult.7346, Cln_Vult.7508, Cln_Vult.7572, Cln_Vult.7573, 
   Cln_Vult.7574, Cln_Vult.7576, Cln_Vult.7577, Cln_Vult.7578, Cln_Vult.7587, Cln_Vult.7591, 
   Cln_Vult.7593, Cln_Vult.7615, Cln_Vult.7616, Cln_Vult.7641, Cln_Vult.7642, Cln_Vult.8108)

## 2. OPTIONAL: Downscale the movement data to a lower resolution --------------------------------------
### Filter the data to 1 hour intervals
Cln_Vult.All.1h <- Cln_Vult.All %>% mt_filter_per_interval(criterion = "first",unit = "hour")

## OR
### Filter the data to 15 minute intervals
Cln_Vult.All.15m <- Cln_Vult.All %>% mt_filter_per_interval(criterion = "first",unit = "15 minutes")

## 3. Save the resulting stacked tracks ----------------------------------------------------------------
### For the complete move2 object
save(Cln_Vult.All, file = "Data_Cleaned/Mv2.AllVult.ClnLocs.Rdata")
## OR
### For the 1h-scaled move2 object
save(Cln_Vult.All.1h, file = "Data_Cleaned/Mv2.1h.ClnLocs.AllVult.Rdata")
## OR
### For the 15min-scaled move2 object
save(Cln_Vult.All.15m, file = "Data_Cleaned/Mv2.15min.ClnLocs.AllVult.Rdata")