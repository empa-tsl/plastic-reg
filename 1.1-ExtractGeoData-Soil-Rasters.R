# This script extracts information from raster-like geodatasets available online
# on the website of the Swiss Federal Statistical Office (FSO).
# The paths to the original data and the location for saving the processed data
# should be adapted.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 02.10.2019


library("raster")
library("rgdal")
library("sp")
library("sf")
library("xlsx")

##### IMPORT SIMPLE DATA ##########################################################################

# create a list for storing the modified preliminary data
Data <- list()

### Extract the useful data from the different geodatasets

# Population
Data[["Population"]] <- read.csv(file="STATPOP2014G.csv", header=TRUE, sep=",")
Data[["Population"]] <- Data[["Population"]][,c("E_KOORD", "N_KOORD", "B14BTOT")]
colnames(Data[["Population"]]) <- c("XCOORD", "YCOORD", "DATA")

# Buildings
Data[["Buildings"]] <- read.csv(file="GWS2010G.csv", header=TRUE, sep=",")
Data[["Buildings"]] <- Data[["Buildings"]][,c("E_KOORD", "N_KOORD", "G10B10")]
Data[["Buildings"]][is.na(Data[["Buildings"]][,"G10B10"]),"G10B10"] <- 0
colnames(Data[["Buildings"]]) <- c("XCOORD", "YCOORD", "DATA")

# Industry
Data[["Industry"]] <- read.csv(file="gd-b-00.03-37-noas04G/AREA_NOAS04_17_161114.csv", header=TRUE, sep=",")
Data[["Industry"]] <- Data[["Industry"]][,c("X", "Y", "AS09_17")]
Data[["Industry"]][which(!Data[["Industry"]][,3] %in% c(1,4)),3] <- 0
Data[["Industry"]][which(Data[["Industry"]][,3] %in% c(1,4)),3] <- 1
colnames(Data[["Industry"]]) <- c("XCOORD", "YCOORD", "DATA")

# Agriculture
Data[["Agriculture"]] <- read.csv(file="gd-b-00.03-37-noas04G/AREA_NOAS04_17_161114.csv", header=TRUE, sep=",")
Data[["Agriculture"]] <- Data[["Agriculture"]][,c("X", "Y", "AS09_17")]
Data[["Agriculture"]][which(!Data[["Agriculture"]][,3] %in% c(6,7)),3] <- 0
Data[["Agriculture"]][which(Data[["Agriculture"]][,3] %in% c(6,7)),3] <- 1
colnames(Data[["Agriculture"]]) <- c("XCOORD", "YCOORD", "DATA")

# Natural soil
Data[["NaturalSoil"]] <- read.csv(file="gd-b-00.03-37-noas04G/AREA_NOAS04_17_161114.csv", header=TRUE, sep=",")
Data[["NaturalSoil"]] <- Data[["NaturalSoil"]][,c("X", "Y", "AS09_17")]
Data[["NaturalSoil"]][which(!Data[["NaturalSoil"]][,3] %in% c(8,10,11,12,15,16)),3] <- 0
Data[["NaturalSoil"]][which(Data[["NaturalSoil"]][,3] %in% c(8,10,11,12,15,16)),3] <- 1
colnames(Data[["NaturalSoil"]]) <- c("XCOORD", "YCOORD", "DATA")

##### RASTERIZE ###################################################################################

# change the coordinate reference system (CRS)
for(i in 1:length(Data)){
  if(names(Data)[i] %in% c("Industry","Agriculture","NaturalSoil")){
    Data[[i]][,"XCOORD"] <- Data[[i]][,"XCOORD"] + 2000000
    Data[[i]][,"YCOORD"] <- Data[[i]][,"YCOORD"] + 1000000
  }
}

# find the maximum extent of all rasters
xmin <- min(sapply(Data, function(x) min(x[[1]])))
xmax <- max(sapply(Data, function(x) max(x[[1]])))
ymin <- min(sapply(Data, function(x) min(x[[2]])))
ymax <- max(sapply(Data, function(x) max(x[[2]])))

# create an extent object to apply to all rasters
ext <- extent(xmin, xmax, ymin, ymax)

# path for saving final data
output.path <- "GIVE_FULL_PATH_HERE"

for(i in 1:length(Data)){
  
  # create a raster from point data
  rasterData <- rasterFromXYZ(Data[[i]],
                              crs = CRS("+init=epsg:2056"),
                              res = c(100,100),
                              digits = 5)
  
  # extend and crop raster so that it is consistent with all other rasters
  rasterData.ext <- extend(rasterData, ext)
  rasterData.ext <- crop(rasterData, ext)
  
  # store (if want to change the resolution, use next line instead)
  Rasters <- rasterData.ext
  
  # # decrease resolution from 100m to 1km
  # Rasters <- aggregate(rasterData.ext, fun=sum, fact=10)
  
  # export raster (do writeFormats() to find out formats available)
  writeRaster(Rasters,
              paste0(output.path,names(Data)[i]),
              format = "GTiff",
              overwrite=TRUE)
  
  # remove for memory
  rm(Rasters)
}
