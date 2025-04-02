#Title #################################################################################
# T.VULTURE MOVE ANALYSIS. M1: BIRDSCARIBBEAN 2024. TRACK AND MCP HOME RANGE ANALYSIS ##
#Function ##############################################################################
## Map the movement tracks of a single animal, and export it as a shapefile. 
## Then, calculate and map its Home Range using the MCP95% method and export it as a shapefile.  


## Packages

library(move2)
library(dplyr)
library(mapview)
library(adehabitatHR)


## 0. Load the Data ------------------------------------------------------------------------------------
### Sequentially load the file for each Vulture and run the Script for each.
### IMPORTANT: CHANGE the file name of each Vulture to be consistent with the animal ID
load("Data_Cleaned/Mv2.ClnLocs.Vult7198.Rdata")


## 1. Map the movement tracks --------------------------------------------------------------------------
### Visualize the movement data of a single individuals as segments
mapView(mt_track_lines(TVult.mv2_loc), zcol="individual-local-identifier", legend=F)

### Save the Movement Track as a Vector file to be used in a GIS
#### CHANGE the 'layer =' Argument value
sf::st_write(mt_track_lines(TVult.mv2_loc),
             dsn = "Data_Processed/GIS",
             layer = "Track.7198",
             driver = "ESRI Shapefile")

## 2. Divide the track between Wet and Dry seasons -----------------------------------------------------
### Dry seasons span from November-April and Wet seasons from May-October
#### NOTE: Vulture XXXX move2 file does not have a "study-local-timestamp" variable, 
##### the filter should be changed to use the variable "timestamp", use the code below this chunk.
TVult.mv2_loc.Dry1 <- TVult.mv2_loc %>% filter(`study-local-timestamp` >  "2021-01-13" 
                                             & `study-local-timestamp` <  "2021-05-01")
TVult.mv2_loc.Wet1 <- TVult.mv2_loc %>% filter(`study-local-timestamp` >= "2021-05-01" 
                                             & `study-local-timestamp` <  "2021-11-01")
TVult.mv2_loc.Dry2 <- TVult.mv2_loc %>% filter(`study-local-timestamp` >= "2021-11-01" 
                                             & `study-local-timestamp` <  "2022-05-01")
TVult.mv2_loc.Wet2 <- TVult.mv2_loc %>% filter(`study-local-timestamp` >= "2022-05-01" 
                                             & `study-local-timestamp` <  "2022-11-01")
TVult.mv2_loc.Dry3 <- TVult.mv2_loc %>% filter(`study-local-timestamp` >= "2022-11-01" 
                                             & `study-local-timestamp` <  "2023-05-01")
TVult.mv2_loc.Wet3 <- TVult.mv2_loc %>% filter(`study-local-timestamp` >= "2023-05-01" 
                                             & `study-local-timestamp` <  "2023-11-01")
TVult.mv2_loc.Dry4 <- TVult.mv2_loc %>% filter(`study-local-timestamp` >= "2023-11-01" 
                                             & `study-local-timestamp` <  "2024-05-01")
TVult.mv2_loc.Wet4 <- TVult.mv2_loc %>% filter(`study-local-timestamp` >= "2024-05-01" 
                                             & `study-local-timestamp` <  "2024-11-01")

# ### ADDITIONAL. As stated above, Vulture XXXX does not have a "study-local-timestamp" variable.
# TVult.mv2_loc.Dry1 <- TVult.mv2_loc %>% filter(`timestamp` >  "2021-01-13" 
#                                              & `timestamp` < "2021-05-01")
# TVult.mv2_loc.Wet1 <- TVult.mv2_loc %>% filter(`timestamp` >= "2021-05-01" & `timestamp` < "2021-11-01")
# TVult.mv2_loc.Dry2 <- TVult.mv2_loc %>% filter(`timestamp` >= "2021-11-01" & `timestamp` < "2022-05-01")
# TVult.mv2_loc.Wet2 <- TVult.mv2_loc %>% filter(`timestamp` >= "2022-05-01" & `timestamp` < "2022-11-01")
# TVult.mv2_loc.Dry3 <- TVult.mv2_loc %>% filter(`timestamp` >= "2022-11-01" & `timestamp` < "2023-05-01")
# TVult.mv2_loc.Wet3 <- TVult.mv2_loc %>% filter(`timestamp` >= "2023-05-01" & `timestamp` < "2023-11-01")
# TVult.mv2_loc.Dry4 <- TVult.mv2_loc %>% filter(`timestamp` >= "2023-11-01" & `timestamp` < "2024-05-01")
# TVult.mv2_loc.Wet4 <- TVult.mv2_loc %>% filter(`timestamp` >= "2024-05-01" & `timestamp` < "2024-11-01")


### Join the data from each season in a single object
Tvult.mv2.Wet <- rbind.data.frame(TVult.mv2_loc.Wet1, TVult.mv2_loc.Wet2, 
                                  TVult.mv2_loc.Wet3, TVult.mv2_loc.Wet4)
Tvult.mv2.Dry <- rbind.data.frame(TVult.mv2_loc.Dry1, TVult.mv2_loc.Dry2, 
                                  TVult.mv2_loc.Dry3, TVult.mv2_loc.Dry4)

## 3. Calculate and export the MCP for each season -----------------------------------------------------
Tvult.mv2.Wet$id <- mt_track_id(Tvult.mv2.Wet)
Tvult.mv2.Wet_id <- sf::as_Spatial(Tvult.mv2.Wet[,'id'])
Tvult.mv2.Dry$id <- mt_track_id(Tvult.mv2.Dry)
Tvult.mv2.Dry_id <- sf::as_Spatial(Tvult.mv2.Dry[,'id'])

mcp_Vult.Wet <- adehabitatHR::mcp(as(Tvult.mv2.Wet_id,"SpatialPoints"))
mcp_Vult.Dry <- adehabitatHR::mcp(as(Tvult.mv2.Dry_id,"SpatialPoints"))
raster::shapefile(mcp_Vult.Wet, filename = "Data_Processed/GIS/MCP.Wet.8108.shp")
raster::shapefile(mcp_Vult.Dry, filename = "Data_Processed/GIS/MCP.Dry.8108.shp")

rm(Tvult.mv2.Dry, Tvult.mv2.Wet, TVult.mv2_loc.Dry1, TVult.mv2_loc.Dry2, 
   TVult.mv2_loc.Dry3, TVult.mv2_loc.Dry4, TVult.mv2_loc.Wet1, TVult.mv2_loc.Wet2, 
   TVult.mv2_loc.Wet3, TVult.mv2_loc.Wet4, Tvult.mv2.Wet_id, Tvult.mv2.Dry_id, 
   mcp_Vult.Wet, mcp_Vult.Dry)