# This script adds the regionalized layer per environmental compartment, plastic size
# and material. For water only.
# The paths to the original data and the location for saving the processed data
# should be adapted.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 02.10.2019

Materials <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")

# load water data
load("Data_Regionalized/Water.Rdata")

### ADD ALL EMISSIONS BY POLYMER

for(mat in Materials){
  message(paste(format(Sys.time(), "%H:%M:%S"),"Adding data for",mat))
  # loop over size of plastic
  for(size in c("Water", "WaterMP")){
    
    for(type in c("", "_Q05", "_Q95")){
      # summed layer name without parentheses (doesn't work for plotting later)
      layername <- paste0(mat,"_",gsub(" ","", gsub("\\)","", gsub("\\(","", size))), type)
      
      ### lines and poly layers
      
      # layer names of the layers to add (for both rivers and lakes)
      layer1 <- paste0(mat, "_UrbanWater_",   size, type)
      layer2 <- paste0(mat, "_NaturalWater_", size, type)
      layer3 <- paste0(mat, "_CSO_",          size, type)
      layer4 <- paste0(mat, "_WWTPsec_",      size, type)
      layer5 <- paste0(mat, "_WWTPter_",      size, type)
      
      # rivers in kg/km
      if(layer1 %in% names(rivers_emi)){ values1 <- rivers_emi[layer1][[1]] } else { values1 <- rep(0,length(rivers_emi$geometry)) }
      if(layer2 %in% names(rivers_emi)){ values2 <- rivers_emi[layer2][[1]] } else { values2 <- rep(0,length(rivers_emi$geometry)) }
      if(layer3 %in% names(rivers_emi)){ values3 <- rivers_emi[layer3][[1]] } else { values3 <- rep(0,length(rivers_emi$geometry)) }
      if(layer4 %in% names(rivers_emi)){ values4 <- rivers_emi[layer4][[1]] } else { values4 <- rep(0,length(rivers_emi$geometry)) }
      if(layer5 %in% names(rivers_emi)){ values5 <- rivers_emi[layer5][[1]] } else { values5 <- rep(0,length(rivers_emi$geometry)) }
      
      rivers_emi[layername] <- ( values1 + values2 + values3 + values4 + values5 )
      
      # lakes in kg/ha
      if(layer1 %in% names(lakes_emi)){ values1 <- lakes_emi[layer1][[1]] } else { values1 <- rep(0,length(lakes_emi$geometry)) }
      if(layer2 %in% names(lakes_emi)){ values2 <- lakes_emi[layer2][[1]] } else { values2 <- rep(0,length(lakes_emi$geometry)) }
      if(layer3 %in% names(lakes_emi)){ values3 <- lakes_emi[layer3][[1]] } else { values3 <- rep(0,length(lakes_emi$geometry)) }
      if(layer4 %in% names(lakes_emi)){ values4 <- lakes_emi[layer4][[1]] } else { values4 <- rep(0,length(lakes_emi$geometry)) }
      if(layer5 %in% names(lakes_emi)){ values5 <- lakes_emi[layer5][[1]] } else { values5 <- rep(0,length(lakes_emi$geometry)) }
      
      lakes_emi[ layername] <- ( values1 + values2 + values3 + values4 + values5 )
      
    }
  }
}

save(lakes_emi, lakes_notCH, rivers_emi, file = "Data_Summed/Water.Rdata")
