# This script performs the regionalization for water.
# The paths to the original data and the location for saving the processed data
# should be adapted.
# 
# Author: Delphine Kawecki-Wenger
# Date of last modification: 02.10.2019

load("Data_Modified/SurfaceWaters_wPoints.Rdata")
Materials <- c("LDPE", "HDPE", "PP", "PS", "EPS", "PVC", "PET")

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



# loop over proxies
proxies.water <- c("Population", "NaturalSoil", "WWTPsec", "WWTPter", "CSO")
for(prox in proxies.water){
  
  for(mat in Materials){
    
    # loop over size of plastic
    for(envcomp in c("Water", "Water (MP)")){
      
      # store the emission value
      emi    <- EmissionFlows[[mat]][envcomp,prox]
      emi.95 <- EmissionFlows.Q95[[mat]][envcomp,prox]
      emi.05 <- EmissionFlows.Q05[[mat]][envcomp,prox]
      
      # if not NA, calculate the regionalized flows
      if(!is.na(emi)){
        
        # calculate the total for normalization
        Total <- sum(rivers_emi[prox][[1]], lakes_emi[prox][[1]])
        
        # calculate emission
        temp.river    <- rivers_emi[prox][[1]]/Total*emi
        temp.river.05 <- rivers_emi[prox][[1]]/Total*emi.05
        temp.river.95 <- rivers_emi[prox][[1]]/Total*emi.95
        
        temp.lake     <- lakes_emi[prox][[1]]/Total*emi
        temp.lake.05  <- lakes_emi[prox][[1]]/Total*emi.05
        temp.lake.95  <- lakes_emi[prox][[1]]/Total*emi.95
        
        # for a nice name without parentheses (doesn't work for plotting later)
        layername    <- paste0(mat,"_",prox,"_",gsub(" ","", gsub("\\)","", gsub("\\(","", envcomp))))
        layername.05 <- paste0(mat,"_",prox,"_",gsub(" ","", gsub("\\)","", gsub("\\(","", envcomp))),"_Q05")
        layername.95 <- paste0(mat,"_",prox,"_",gsub(" ","", gsub("\\)","", gsub("\\(","", envcomp))),"_Q95")
        
        # divide by length of segment for river or area for lake
        # give in kg/km (original units: t/m == 1000 kg / 0.001 km = 10^6 kg/km)
        rivers_emi[layername]    <- temp.river/rivers_emi$Shape_Length *10^6
        rivers_emi[layername.05] <- temp.river.05/rivers_emi$Shape_Length *10^6
        rivers_emi[layername.95] <- temp.river.95/rivers_emi$Shape_Length *10^6
        
        # give in kg/ha (original units: t/m2 == 1000kg / 0.0001ha == 10^7 kg/ha)
        lakes_emi[layername]    <- temp.lake/lakes_emi$Shape_Area *10^7
        lakes_emi[layername.05] <- temp.lake.05/lakes_emi$Shape_Area *10^7
        lakes_emi[layername.95] <- temp.lake.95/lakes_emi$Shape_Area *10^7
        
      }
      
    }
    
  }
  
}


# save
save(lakes_emi, lakes_notCH, rivers_emi, file = "Data_Regionalized/Water.Rdata")
