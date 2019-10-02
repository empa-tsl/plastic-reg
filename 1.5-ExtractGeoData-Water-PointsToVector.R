# This script connects the point sources to the river and lakes network.
# The paths to the original data and the location for saving the processed data
# should be adapted.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 02.10.2019

library(sf)

# load vector water data
load("Data_Modified/SurfaceWaters.Rdata")

# load point water data
load("Data_Modified/WWTP_CSO_sf.Rdata")
points_sf <- Data.sf
rm(Data.sf)
st_crs(points_sf) <- "+init=epsg:2056"


### HOW TO HANDLE POINT EMISSIONS

# for each emission point, create a vector of indices giving the nearest feature
# with lakes
ind.lakes <- st_nearest_feature(points_sf, lakes_emi)

# with rivers
ind.rivers <- st_nearest_feature(points_sf, rivers_emi)

# calculate minimal distance with lakes
dist.lakes <- rep(NA, length(ind.lakes))
for(point in 1:length(points_sf$geometry)){
  dist.lakes[point] <- st_distance(points_sf[point,], lakes_emi[ind.lakes[point],])
}

# calculate minimal distance with rivers
dist.rivers <- rep(NA, length(ind.rivers))
for(point in 1:length(points_sf$geometry)){
  dist.rivers[point] <- st_distance(points_sf[point,], rivers_emi[ind.rivers[point],])
}

# compare (= create a logical vector, TRUE if river is closer, FALSE if lake is closer)
log.river.closer <- (dist.rivers < dist.lakes)

# create a layer in the river or lake sf object corresponding to the points in the right order
add.layernames <- names(points_sf)[1:3]

for(lay in add.layernames){
  lakes_emi[lay] <- rep(0,length(lakes_emi$geometry))
  rivers_emi[lay] <- rep(0,length(rivers_emi$geometry))
}


### First: rivers

# create a nested empty list (first emission ayers; then river segments)
addpoint <- sapply(add.layernames, function(x) NULL)
for(i in 1:length(addpoint)){ addpoint[[i]] <- rep(0,length(rivers_emi$geometry)) }

# store point emission to layer (whole loop takes about 50 minutes)
message(paste(format(Sys.time(), "%H:%M:%S"),"started rivers"))
for(seg in 1:length(rivers_emi$geometry)){
  
  # find point that needs to be added to the river segment
  ind.point <- which(seg == ind.rivers)
  
  # skip if river segment is never the closest to an emission point
  if(length(ind.point) == 0){
    next
    
  } else if(length(ind.point) == 1){ # if only one point to add, easy
    
    # check if the distance between point and lake is smaller, if yes, skip (it will be added to a lake instead)
    if(!log.river.closer[ind.point]){ next }
    
    # else, add the point to the river segment (except if NA)
    for(lay in add.layernames){
      if(!is.na(points_sf[lay][[1]][ind.point])){
        addpoint[[lay]][ind.rivers[ind.point]] <- addpoint[[lay]][ind.rivers[ind.point]] + points_sf[lay][[1]][ind.point]
      }
    }
    
  } else { # if more than one point to add per segment, loop over points and do the same
    
    for(ip in ind.point){
      
      # check if the distance between point and lake is smaller, if yes, skip (it will be added to a lake instead)
      if(!log.river.closer[ip]){ next }
      
      # else, add the point to the river segment (except if NA)
      for(lay in add.layernames){
        if(!is.na(points_sf[lay][[1]][ip])){
          addpoint[[lay]][ind.rivers[ip]] <- addpoint[[lay]][ind.rivers[ip]] + points_sf[lay][[1]][ip]
        }
      }
      
    }
    
  }
}
message(paste(format(Sys.time(), "%H:%M:%S"),"finished rivers"))

# add stored emissions to final sf object
for(lay in add.layernames){
  rivers_emi[lay] <- addpoint[[lay]]
}



### Second: lakes

# create a nested empty list (first lemission ayers; then lakes)
addpoint <- sapply(add.layernames, function(x) NULL)
for(i in 1:length(addpoint)){ addpoint[[i]] <- rep(0,length(lakes_emi$geometry)) }

# store point emission to layer (whole loop takes about 3-50 minutes)
message(paste(format(Sys.time(), "%H:%M:%S"),"started lakes"))
for(seg in 1:length(lakes_emi$geometry)){
  
  # find point that needs to be added to the lake
  ind.point <- which(seg == ind.lakes)
  
  # skip if lake is never the closest to an emission point
  if(length(ind.point) == 0){
    next
    
  } else if(length(ind.point) == 1){ # if only one point to add, easy
    
    # check if the distance between point and lake is smaller, if not, skip
    if(log.river.closer[ind.point]){ next }
    
    # else, add the point to the lake (except if NA)
    for(lay in add.layernames){
      if(!is.na(points_sf[lay][[1]][ind.point])){
        addpoint[[lay]][ind.lakes[ind.point]] <- addpoint[[lay]][ind.lakes[ind.point]] + points_sf[lay][[1]][ind.point]
      }
    }
    
  } else { # if more than one point to add per segment, loop over points and do the same
    
    for(ip in ind.point){
      
      # check if the distance between point and lake is smaller, if yes, skip (it will be added to a lake instead)
      if(log.river.closer[ip]){ next }
      
      # else, add the point to the lake (except if NA)
      for(lay in add.layernames){
        if(!is.na(points_sf[lay][[1]][ip])){
          addpoint[[lay]][ind.lakes[ip]] <- addpoint[[lay]][ind.lakes[ip]] + points_sf[lay][[1]][ip]
        }
      }
      
    }
    
  }
}
message(paste(format(Sys.time(), "%H:%M:%S"),"finished lakes"))

# add stored emissions to final sf object
for(lay in add.layernames){
  lakes_emi[lay] <- addpoint[[lay]]
}



### check addition is correct
for(lay in add.layernames){
  sumpoints <- sum(points_sf[lay][[1]], na.rm = T)
  sumpolys  <- sum(rivers_emi[lay][[1]], na.rm = T) + sum(lakes_emi[lay][[1]], na.rm = T)
  stopifnot( sumpoints == sumpolys )
}

### save
save(lakes_emi, lakes_notCH, rivers_emi, file = "Data_Modified/SurfaceWaters_wPoints.Rdata")
