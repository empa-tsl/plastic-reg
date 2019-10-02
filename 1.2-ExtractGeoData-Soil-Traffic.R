# This script extracts the relevant data from the traffic geodataset (SonBASE) available on
# request at the Swiss Federal Office for the Environment (FOEN).
# The paths to the original data and the location for saving the processed data
# should be adapted.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 02.10.2019

library("sf")


# load geodatabase using sf
traffic <- st_read(dsn = "Data_Raw/Traffic/sonBASE/sonBASE_Verkerhsdaten2015.gdb",
                   layer = "OSM_Traffic")

# keep only interesting data
traffic <- traffic[c("osm_id", "DTV_00_24", "tunnel")]
                   
# remove tunnels
ind <- traffic[["tunnel"]]
traffic[["DTV_00_24"]][as.logical(ind)] <- 0

# save as shapefile
write_sf(traffic, "Data_Raw/Traffic/sonBASE/Traffic.shp")

# the rest is done in QGIS
warning("Transformation to raster made in QGIS: vector to raster using population.tif")