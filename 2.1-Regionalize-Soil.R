# This script performs the regionalization for soil and air (so exclusively rasters)
# The paths to the original data and the location for saving the processed data
# should be adapted.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 02.10.2019

library("xlsx")
library("raster")

Materials <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")

# proxies which are not rasters, to skip
notrasters <- c("WWTPsec", "WWTPter", "CSO", "NaturalWater", "UrbanWater")

# import emission flows ( ---- IN TONNES ---- )
EmissionFlows <- EmissionFlows.Q05 <- EmissionFlows.Q95 <- sapply(Materials, function(x) NULL)
for(mat in Materials){
  
  EmissionFlows[[mat]]     <- read.csv(file = paste0("Data_Modified/EmissionFlows_Sum_Mean_",mat,".csv"),
                                       header = TRUE,
                                       sep = ",",
                                       row.names = 1)
  
  EmissionFlows.Q95[[mat]] <- read.csv(file = paste0("Data_Modified/EmissionFlows_Sum_Q95_",mat,".csv"),
                                       header = TRUE,
                                       sep = ",",
                                       row.names = 1)
  
  EmissionFlows.Q05[[mat]] <- read.csv(file = paste0("Data_Modified/EmissionFlows_Sum_Q05_",mat,".csv"),
                                       header = TRUE,
                                       sep = ",",
                                       row.names = 1)
}

# get the environmental compartments and available proxies
env.comp <- unique(c(sapply(EmissionFlows,rownames)))
proxies <- unique(c(sapply(EmissionFlows,colnames)))

# path for saving final data
output.path <- "C:/Users/dew/Documents/Polybox/Projects/2019_Regionalization_CH/Data_Regionalized/"

# load rasters
Rasters <- list()
for(prox in proxies){
  if(prox %in% notrasters){
    next
  }
  Rasters[[prox]] <- raster(paste0("Data_Modified/",prox,".tif"))
}

# get maximal extent
xmax.all <- max(sapply(sapply(Rasters,extent), function(x) x@xmax))
xmin.all <- min(sapply(sapply(Rasters,extent), function(x) x@xmin))
ymax.all <- max(sapply(sapply(Rasters,extent), function(x) x@ymax))
ymin.all <- min(sapply(sapply(Rasters,extent), function(x) x@ymin))
ext <- extent(xmin.all, xmax.all, ymin.all, ymax.all)

# change extent of all rasters
for(i in 1:length(Rasters)){
  Rasters[[i]] <- extend(Rasters[[i]], ext)  
}

# loop over materials
for(mat in Materials){
  
  message(paste(format(Sys.time(), "%H:%M:%S"),"Regionalizing for",mat))
  
  # store emission flows
  em    <- EmissionFlows[[mat]]*1000 # in kg
  em.05 <- EmissionFlows.Q05[[mat]]*1000 # in kg
  em.95 <- EmissionFlows.Q95[[mat]]*1000 # in kg
  
  # loop over environmental compartments
  for(env in env.comp){
    
    # loop over proxies
    for(prox in proxies){
      
      # if the data needed for the regionalization is not a raster, skip
      if(prox %in% notrasters){
        next
      }
      
      # if the proxy is not needed for the considered environmental compartment, skip
      if(is.na(em[env,prox])){
        next
      }
      
      # find relevant map
      map <- map.05 <- map.95 <- Rasters[[prox]]
      
      # regionalize the emission
      map[]    <- map[]   /sum(map[],    na.rm=T) * em[env,prox]
      map.05[] <- map.05[]/sum(map.05[], na.rm=T) * em.05[env,prox]
      map.95[] <- map.95[]/sum(map.95[], na.rm=T) * em.95[env,prox]
      
      # create a name for the map
      mapname    <- paste0(gsub("\\)","",gsub("\\(","",gsub(" ","",env))),"_",prox)
      mapname.05 <- paste0(gsub("\\)","",gsub("\\(","",gsub(" ","",env))),"_",prox,"_05")
      mapname.95 <- paste0(gsub("\\)","",gsub("\\(","",gsub(" ","",env))),"_",prox,"_95")
      
      # check that the map still has the right extent
      stopifnot(ext == extent(map), ext == extent(map.05), ext == extent(map.95))
      
      # export raster (do writeFormats() to find out formats available)
      writeRaster(map,
                  paste0(output.path, mat, "_", mapname),
                  format = "GTiff",
                  overwrite=TRUE)
      writeRaster(map.05,
                  paste0(output.path, mat, "_", mapname.05),
                  format = "GTiff",
                  overwrite=TRUE)
      writeRaster(map.95,
                  paste0(output.path, mat, "_", mapname.95),
                  format = "GTiff",
                  overwrite=TRUE)
      
    }
  }
  
  message(paste(format(Sys.time(), "%H:%M:%S"),"Done."))
}

