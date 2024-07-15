#Title ######################################################################
# T.VULTURE MOVE ANALYSIS. Script 04: SPACE USE AND AREA CENTERED ANALYSIS ##
#Function ###################################################################
## Performs geometric analysis on the areas described by the movement.
## Calculate Home Ranges based on different methods.

# `Move2` (New) Workflow ----------------------------------------------------------------------------------
## Packages

library(move2)
library(sf)
library(raster)
library(adehabitatHR)

## 0. Load the cleaned data -------------------------------------------------------------------------------
load(choose.files()) ## Select the file from the Cleaned Data folder

## 1. 
###

### NOTE: This code was taken verbatim from the "lesson6.2023 Space Use" Animove '23 Class Script.
######### It was altered for generation of a MCP for all VUltures, using all trajectories stacked in a single 'move2' object.
Cln_Vult.All$id <- mt_track_id(Cln_Vult.All)
Cln_Vult.All_sp <- as_Spatial(Cln_Vult.All[,'id'])

# function mcp is very particular about the input object, it must only
# contain 1 column, and names of the individuals names have to follow
# the validNames() rules
Cln_Vult.All_sp <- Cln_Vult.All_sp[,(names(Cln_Vult.All_sp) %in% "id")]
levels(Cln_Vult.All_sp$id) <- validNames(levels(Cln_Vult.All_sp$id))

Cuba.7198 <- Cln_Vult.All_sp[Cln_Vult.All_sp$id=="Cuba.7198",]
# calculate mcp:
# by default, this package excludes the 5% locations farthest from the arithmetic mean
mcp.Cuba.all <- adehabitatHR::mcp(as(Cln_Vult.All_sp,"SpatialPoints"))
raster::plot(Cln_Vult.All_sp, type="n", xlab="Longitude", ylab="Latitude")
raster::plot(mcp.Cuba.all, col="grey90", lty=2, lwd=1.25, add=TRUE)
points(Cuba.7198, pch=16)
points(Cuba.7198, pch=1, col="white")
legend("topright", as.character("95% MCP"), fill="grey90", bty="n")
mcp.Cuba.7198
# Note: area value seems strange. That is because our used locations are in the geographic coordinates system (long/lat). adehabitatHR calculated the area according to the units of the projection, in this case decimal degrees

# Therefore we have to project our data into a equidistant projection
#  We take again the median of the coordinates to center our projection
apply(st_coordinates(Cln_Vult.All), 2, median)
AEQ_1p <- "+proj=aeqd +lat_0=22.38059 +lon_0=-79.26601 +x_0=0 +y_0=0 +R=6371000 +units=m +no_defs +type=crs"
habitatAEQ <- sf::st_transform(Cln_Vult.All, AEQ_1p)

library(rgeos)
#first option: reproject locations, than calculate mcp
Cln_Vult.All_sp.proj <- spTransform(Cln_Vult.All_sp, AEQ_1p)
mcpData.proj <- adehabitatHR::mcp(Cln_Vult.All_sp.proj)
#second option: calculate mcp, than reproject mcp
mcpData <- mcp(Cln_Vult.All_sp)
projection(mcpData) <- CRS("+proj=longlat +datum=WGS84") #declare original latlong proj before reprojecting
mcpData <- spTransform(mcpData, AEQ_1p)
plot(Cln_Vult.All_sp.proj[Cln_Vult.All_sp.proj$id=="X021",], bty="na", xlab="Longitude", ylab="Latitude")
plot(mcpData.proj[mcpData.proj$id=="X021",], add=TRUE)
plot(mcpData[mcpData$id=="X021",], add=TRUE, lty=2)
legend("bottomleft", c("First reproject then mcp", "First mcp then reproject"), lty=c(1,2), bty="n")
legend("topleft", sprintf("Area = %.2f", c(rgeos::gArea(mcpData.proj, byid=TRUE)["X021"],
                                           rgeos::gArea(mcpData, byid=TRUE)["X021"])/1000^2), lty=c(1,2), bty="n")
# Note to plot:
# - the 2 options result in different area calculations
# - adehabitatHR uses distance between locations to do the calculation
# - therefore always: FIRST project locations, and THEN calculate MCP


## Size of MCP changes with sampling effort or sampling size
library(move)
bats_mv <- to_move(Cln_Vult.All)
hrBootstrap(bats_mv[['X021']], rep=500, levelMax=95)
legend("bottomright", legend=c("MCP size with all data","100% percentil","75% percentil","50% percentil","25% percentil","0% percentil"), 
       lty=c(4,2,3,1,3,2), col=c("black","cyan","red","black","red","cyan"), bty="n")
# Note to plot:
# - if sampling is large enough a saturation between sample size and area is reached

# In conclusion:
# simple and intuitive
# but does not account for intensity of utilization
# assumes independence of locations



# `Move` (Old) Workflow -----------------------------------------------------------------------------------
## Packages 
library(move)
library(raster)
library(adehabitatHR)
library(dplyr)

## 0. Load the cleaned data -------------------------------------------------------------------------------
load(choose.files()) ## Select the file from the Cleaned Data folder
