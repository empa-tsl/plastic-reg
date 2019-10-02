# This script adds the regionalized layer per environmental compartment, plastic size
# and material. For soil and air only.
# The paths to the original data and the location for saving the processed data
# should be adapted.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 02.10.2019

library("raster")

Materials <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")

output.path <- "FULL_PATH_TO_OUTPUT_HERE"

for(mat in Materials){
  
  message(paste(format(Sys.time(), "%H:%M:%S"),"Adding rasters for",mat))
  
  # generate a list of input rasters
  grids <- list.files(pattern = mat)
  grids <- grids[!grepl("pdf",grids)]
  
  # make a separate list for the quantiles
  grids.05 <- grids[grepl("_05",grids)]
  grids.95 <- grids[grepl("_95",grids)]
  
  # make a separate list for the mean
  grids <- grids[!grepl("5",grids)]
  
  # find grids with specific environmental compartment
  grids.soil   <- grids[grepl("Soil",   grids) & !grepl("SoilMP",grids)]
  grids.soilMP <- grids[grepl("SoilMP", grids)]
  grids.air    <- grids[grepl("AirMP",  grids)]
  
  grids.soil.05   <- grids.05[grepl("Soil",   grids.05) & !grepl("SoilMP",grids.05)]
  grids.soilMP.05 <- grids.05[grepl("SoilMP", grids.05)]
  grids.air.05    <- grids.05[grepl("AirMP",  grids.05)]
  
  grids.soil.95   <- grids.95[grepl("Soil",   grids.95) & !grepl("SoilMP",grids.95)]
  grids.soilMP.95 <- grids.95[grepl("SoilMP", grids.95)]
  grids.air.95    <- grids.95[grepl("AirMP",  grids.95)]
  
  # create a raster stack from the input raster files 
  RasterStack.soil   <- stack(grids.soil)
  RasterStack.soilMP <- stack(grids.soilMP)
  RasterStack.air    <- stack(grids.air)
  
  RasterStack.soil.05   <- stack(grids.soil.05)
  RasterStack.soilMP.05 <- stack(grids.soilMP.05)
  RasterStack.air.05    <- stack(grids.air.05)
  
  RasterStack.soil.95   <- stack(grids.soil.95)
  RasterStack.soilMP.95 <- stack(grids.soilMP.95)
  RasterStack.air.95    <- stack(grids.air.95)
  
  stopifnot(grids %in% c(grids.soil, grids.soilMP, grids.air),
            ( length(unique(c(grids.soil, grids.soilMP, grids.air)))
              == length(c(grids.soil, grids.soilMP, grids.air)) ))
  
  # run the sum function on the raster stack - i.e. add (non-cumulatively) the rasters together
  RasterSum.soil   <- sum(RasterStack.soil,   na.rm = T)
  RasterSum.soilMP <- sum(RasterStack.soilMP, na.rm = T)
  RasterSum.air    <- sum(RasterStack.air,    na.rm = T)
  
  RasterSum.soil.05   <- sum(RasterStack.soil.05,   na.rm = T)
  RasterSum.soilMP.05 <- sum(RasterStack.soilMP.05, na.rm = T)
  RasterSum.air.05    <- sum(RasterStack.air.05,    na.rm = T)
  
  RasterSum.soil.95   <- sum(RasterStack.soil.95,   na.rm = T)
  RasterSum.soilMP.95 <- sum(RasterStack.soilMP.95, na.rm = T)
  RasterSum.air.95    <- sum(RasterStack.air.95,    na.rm = T)
  
  # export raster (do writeFormats() to find out formats available)
  writeRaster(RasterSum.soil,   paste0(output.path, mat, "_soil"),   format = "GTiff", overwrite=TRUE)
  writeRaster(RasterSum.soilMP, paste0(output.path, mat, "_soilMP"), format = "GTiff", overwrite=TRUE)
  writeRaster(RasterSum.air,    paste0(output.path, mat, "_air"),    format = "GTiff", overwrite=TRUE)
  
  writeRaster(RasterSum.soil.05,   paste0(output.path, mat, "_soil_Q05"),   format = "GTiff", overwrite=TRUE)
  writeRaster(RasterSum.soilMP.05, paste0(output.path, mat, "_soilMP_Q05"), format = "GTiff", overwrite=TRUE)
  writeRaster(RasterSum.air.05,    paste0(output.path, mat, "_air_Q05"),    format = "GTiff", overwrite=TRUE)
  
  writeRaster(RasterSum.soil.95,   paste0(output.path, mat, "_soil_Q95"),   format = "GTiff", overwrite=TRUE)
  writeRaster(RasterSum.soilMP.95, paste0(output.path, mat, "_soilMP_Q95"), format = "GTiff", overwrite=TRUE)
  writeRaster(RasterSum.air.95,    paste0(output.path, mat, "_air_Q95"),    format = "GTiff", overwrite=TRUE)
  
}