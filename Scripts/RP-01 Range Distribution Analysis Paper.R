# Title
# CUBA T.VULTURE MOVEMENT ANALYSIS. Research Paper 01: Range Distribution Comparison
-------------------------------------------------------------------------------------

# Function
## Map the movement tracks, divide them by seasons and calculate the Home Ranges Distributions using the MCP, KDE and Time-component methods, 
## with the aim of comparing values each other and with the available literature.
---------------------------------------------------------------------------------

## Packages
-----------
library(move2)
library(sf)
library(terra)
library(tidyterra)
library(adehabitatHR)
library(rworldxtra)
library(tidyverse)

## 0. Load the Data ------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
### Load the file with the tracks combined into a single object.
load(choose.files()) ## Search for files in the Data_Cleaned folder.
## NOTE: Individual Cuba-7662 will not be included on the analysis, due to it never going out of the Zoo

## 1. Map the movement tracks --------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------
### Visualize the movement data as segments
data("countriesHigh")
world <- st_as_sf(countriesHigh)
bbox <- st_bbox(TVult.mv2_loc)
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = mt_track_lines(TVult.mv2_loc), 
          aes(color = `individual-local-identifier`)) +
  coord_sf(xlim = c(bbox[1]-2, bbox[3]+2), 
           ylim = c(bbox[2]-2, bbox[4]+2), 
           expand = F) +
theme_void()

### Save the Movement Track as a Vector file for use on a GIS
sf::st_write(mt_track_lines(TVult.mv2_loc), 
             dsn = ".",
             layer = "Track.7593",
             driver = "ESRI Shapefile")

## 2. Classify the track between Wet and Dry seasons -----------------------------------------------------
--------------------------------------------------------------------------------------------------------
### Separate the track into periods according to seasons
### Dry season spans from November to April and Wet season from May to October.
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
TVult.mv2_loc.Dry5 <- TVult.mv2_loc %>% filter(`study-local-timestamp` >= "2024-11-01" 
                                             & `study-local-timestamp` <  "2025-05-01")

### Join periods into single 'move2' objects
Tvult.mv2.Wet <- rbind.data.frame(TVult.mv2_loc.Wet1, TVult.mv2_loc.Wet2, 
                                  TVult.mv2_loc.Wet3, TVult.mv2_loc.Wet4)
Tvult.mv2.Dry <- rbind.data.frame(TVult.mv2_loc.Dry1, TVult.mv2_loc.Dry2, 
                                  TVult.mv2_loc.Dry3, TVult.mv2_loc.Dry4,
                                  TVult.mv2_loc.Dry5)

### Remove excess files to free up memory
rm(TVult.mv2_loc.Dry1, TVult.mv2_loc.Dry2, TVult.mv2_loc.Dry3, 
   TVult.mv2_loc.Dry4, TVult.mv2_loc.Dry5, TVult.mv2_loc.Wet1, 
   TVult.mv2_loc.Wet2, TVult.mv2_loc.Wet3, TVult.mv2_loc.Wet4)

## 3. Calculate the Minimum Convex Polygon at 95% of the track -----------------------------------------
--------------------------------------------------------------------------------------------------------
### Coerces the objects into the 'sp' class
Tvult.mv2.Wet$id <- mt_track_id(Tvult.mv2.Wet)
Tvult.mv2.Wet_id <- sf::as_Spatial(Tvult.mv2.Wet[,'id'])
Tvult.mv2.Dry$id <- mt_track_id(Tvult.mv2.Dry)
Tvult.mv2.Dry_id <- sf::as_Spatial(Tvult.mv2.Dry[,'id'])

### Re-project the points to a local equidistant projection
Tvult.mv2.Wet_id_proj <- spTransform(Tvult.mv2.Wet_id, "EPSG:3796") ## Using the NAD27 / "Cuba Sur" projection.
Tvult.mv2.Dry_id_proj <- spTransform(Tvult.mv2.Dry_id, "EPSG:3796")

### Calculate the MCP's from re-projected objects
mcp_Vult.Wet <- mcp(as(Tvult.mv2.Wet_id_proj, "SpatialPoints"), unout = "km2")
mcp_Vult.Dry <- mcp(as(Tvult.mv2.Dry_id_proj, "SpatialPoints"), unout = "km2")

### Extract the MCP area
mcp_Vult.Wet@data[["area"]]
mcp_Vult.Dry@data[["area"]]

### Visual check of the MCP's
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = mt_track_lines(TVult.mv2_loc), 
          aes(color = `individual-local-identifier`)) +
  geom_sf(data = st_as_sf(mcp_Vult.Wet), 
          fill = "aquamarine", alpha = 0.4) +
  geom_sf(data = st_as_sf(mcp_Vult.Dry), 
          fill = "firebrick", alpha = 0.4) +
  coord_sf(xlim = c(bbox[1]-2, bbox[3]+2), 
           ylim = c(bbox[2]-2, bbox[4]+2), 
           expand = FALSE) +
  theme_void()

### Save the MCP's as Shapefiles for use on a GIS
raster::shapefile(mcp_Vult.Wet, filename = "Data_Processed/GIS/MCP95.Wet.7615.shp")
raster::shapefile(mcp_Vult.Dry, filename = "Data_Processed/GIS/MCP95.Dry.7615.shp")

### Remove temporary files
rm(Tvult.mv2.Dry, Tvult.mv2.Dry_id, Tvult.mv2.Dry_id_proj,
   Tvult.mv2.Wet, Tvult.mv2.Wet_id, Tvult.mv2.Wet_id_proj,
   mcp_Vult.Wet, mcp_Vult.Dry)

## 4. Calculate the Kernel Density Estimate at 95% Utilization Distribution ----------------------------
--------------------------------------------------------------------------------------------------------
### Filter the classified tracks to 1 hour intervals to regularize the intervals
Tvult.mv2.Wet.1h <- Tvult.mv2.Wet %>% mt_filter_per_interval(criterion = "first",
                                                             unit = "hour")
Tvult.mv2.Dry.1h <- Tvult.mv2.Dry %>% mt_filter_per_interval(criterion = "first",
                                                             unit = "hour")

### Re-project the filtered points to a local equidistant projection
Tvult.mv2.Wet.1h.proj <- st_transform(Tvult.mv2.Wet.1h, "EPSG:3796") ## Re-projects the NAD27 / "Cuba Sur" projection ##
Tvult.mv2.Dry.1h.proj <- st_transform(Tvult.mv2.Dry.1h, "EPSG:3796")

### Coerces the objects into the 'sp' class
Tvult.mv2.Wet.1h.proj$id <- mt_track_id(Tvult.mv2.Wet.1h.proj)
Tvult.mv2.Wet.1h.proj_id <- as_Spatial(Tvult.mv2.Wet.1h.proj[,'id'])
Tvult.mv2.Dry.1h.proj$id <- mt_track_id(Tvult.mv2.Dry.1h.proj)
Tvult.mv2.Dry.1h.proj_id <- as_Spatial(Tvult.mv2.Dry.1h.proj[,'id'])

### Calculate the KDE's
kernel.Wet <- kernelUD(as(Tvult.mv2.Wet.1h.proj_id, "SpatialPoints"), h="href")
kernel.Dry <- kernelUD(as(Tvult.mv2.Dry.1h.proj_id, "SpatialPoints"), h="href")

### Plot the resulting KDE's and the movement track
ggplot() +
  geom_sf(data = world) +
  geom_spatraster(data = as(kernel.Wet, "SpatRaster"),
                  alpha = 0.8) +
  geom_sf(data = mt_track_lines(TVult.mv2_loc),
          color = "red",
          alpha = 0.4) +
  coord_sf(xlim = c(bbox[1]-2, bbox[3]+2), 
           ylim = c(bbox[2]-2, bbox[4]+2), 
           expand = F) +
  theme_void()

ggplot() +
  geom_sf(data = world) +
  geom_spatraster(data = as(kernel.Dry, "SpatRaster"),
                  alpha = 0.8) +
  geom_sf(data = mt_track_lines(TVult.mv2_loc),
          color = "red",
          alpha = 0.4) +
  coord_sf(xlim = c(bbox[1]-2, bbox[3]+2), 
           ylim = c(bbox[2]-2, bbox[4]+2),
           expand = F) +
  theme_void()

### Extract the KDE Areas
kernel.area(kernel.Wet, percent = 95, unout = "km2")
kernel.area(kernel.Dry, percent = 95, unout = "km2")

### Save the KDE's as a raster for use on a GIS
writeRaster(as(kernel.Wet, "SpatRaster"), filename = "Data_Processed/GIS/KDEhref.Wet.7593.tif")
writeRaster(as(kernel.Dry, "SpatRaster"), filename = "Data_Processed/GIS/KDEhref.Dry.7593.tif")

### Remove excess files to free up memory
rm(kernel.Wet, kernel.Dry, Tvult.mv2.Wet.1h.proj, 
   Tvult.mv2.Dry.1h.proj, Tvult.mv2.Wet.1h.proj_id, Tvult.mv2.Dry.1h.proj_id)
