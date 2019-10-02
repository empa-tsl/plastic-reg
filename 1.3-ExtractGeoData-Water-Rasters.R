# This script creates the proxies for water which are based on rasters from the
# Swiss Federal Office for the Environment (FOEN).
# The paths to the original data and the location for saving the processed data
# should be adapted.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 02.10.2019

library(raster)
library(rasterVis)
require(rgdal)
library(sf)
library(tmap)


### load population raster

Pop <- raster("Population.tif")
Pop[is.na(Pop[])] <- 0

### load natsoil raster

NatSoil <- raster("NaturalSoil.tif")
NatSoil[is.na(NatSoil[])] <- 0

### load river network

# The input file geodatabase
path <- "FULL_PATH_TO_GEODATABASE/EZGG_2015_Jan2019.gdb"
rivers_ini <- readOGR(dsn = path, layer = "vorfluter_abschnitte")

# extract population data along the river network and return as spatial object
# (but only one value per segment)
# consider all raster points within 500m
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Starting extraction for rivers..."))
rivers_pop <- extract(Pop, rivers_ini, sp = T, fun = sum, buffer = 500, na.rm = T)
save(rivers_pop, file = "Data_Modified/Rivers_Population.Rdata")
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Done!"))

# extract natsoil data along the river network and return as spatial object
# (but only one value per segment)
# consider all raster points within 500m
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Starting extraction for rivers..."))
rivers_soil <- extract(NatSoil, rivers_ini, sp = T, fun = sum, buffer = 500, na.rm = T)
save(rivers_soil, file = "Data_Modified/Rivers_NatSoil.Rdata")
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Done!"))

# convert class
rivers_pop_sf  = st_as_sf(rivers_pop)
rivers_soil_sf = st_as_sf(rivers_soil)


### load lakes
path <- "FULL_PATH_TO_GEODATABASE/EZGG_2015_Jan2019.gdb"
lakes_ini <- readOGR(dsn = path, layer = "basisgeometrie")
# convert class
lakes_sf = st_as_sf(lakes_ini)
# drop all polygons which are not lakes
lakes_only <- lakes_sf[lakes_sf$SEE == 1,]
# save all lake polygons which are not in Switzerland
lakes_notCH <- lakes_only[lakes_only$CH == -1,]
# drop all lake polygons which are not in Switzerland
lakes_only <- lakes_only[lakes_only$CH == 1,]
# convert back
lakes_sp <- as(lakes_only, 'Spatial')

# extract population data around the lakes and return as spatial object
# (but only one value per polygon)
# consider all raster points within 500m
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Starting extraction for lakes..."))
lakes_pop <- extract(Pop, lakes_sp, sp = T, fun = sum, buffer = 500, na.rm = T)
save(lakes_pop, file = "Data_Modified/Lakes_Population.Rdata")
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Done!"))

# extract population data around the lakes and return as spatial object
# (but only one value per polygon)
# consider all raster points within 500m
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Starting extraction for lakes..."))
lakes_soil <- extract(NatSoil, lakes_sp, sp = T, fun = sum, buffer = 500, na.rm = T)
save(lakes_soil, file = "Data_Modified/Lakes_NatSoil.Rdata")
message(paste0("\n\n",format(Sys.time(), "%H:%M:%S"), " Done!"))

# convert class
lakes_pop_sf = st_as_sf(lakes_pop)
lakes_soil_sf = st_as_sf(lakes_soil)

# merge
rivers_emi <- rivers_pop_sf
lakes_emi  <- lakes_pop_sf

# add soil layer
rivers_emi$NaturalSoil <- rivers_soil_sf$NaturalSoil
lakes_emi$NaturalSoil  <- lakes_soil_sf$NaturalSoil

# change CRS of lakes_notCH (which was not automatic, since it was not further processed)
# all other objects should have the same CRS
lakes_notCH <- st_transform(lakes_notCH, crs = st_crs(lakes_pop_sf))

### save data
save(lakes_emi,
     lakes_notCH,
     rivers_emi,
     file = "Data_Modified/SurfaceWaters.Rdata")
