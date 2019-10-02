# This script creates the proxies for point sources to water which are based on
# data from various sources.
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
library("jsonlite")


##### WWTP ########################################################################################

# get identification number of WWTPs
All.WWTPs <- read.xlsx("ARA_Liste_August2017_Version_Internet_.xlsx", sheetIndex = 1, encoding = "UTF-8")
WWTP.name <- All.WWTPs$ARANR

# get coordinates of outlets
WWTP.outlet <- read_sf(dsn = "20180502_Data_OFEV_WWTP",
                       layer = "Einleitung")

# create matrix for all required data
WWTP <- matrix(NA,length(WWTP.name),7,
               dimnames = list(WWTP.name, c("ID", "XCOORD", "YCOORD", "XCOORDMOD", "YCOORDMOD", "VOL", "CAT")))
WWTP[,"ID"] <- WWTP.name

# use number of inhabitants connected as proxy for the volume
WWTP[,"VOL"] <- All.WWTPs[,"Eang..2017."]

# remove WWTPs for which no volume is known
WWTP.name <- WWTP.name[-which(is.na(WWTP[,"VOL"]))]
WWTP <- WWTP[-which(is.na(WWTP[,"VOL"])),]

# find coordinates of the WWTPs
for(id in WWTP.name){
  
  # find corresponding WWTP
  wwtp.ind <- which(WWTP.outlet$ARANR == id)
  
  # if WWTP not available in coordinates dataset, skip for now
  if(length(wwtp.ind) == 0){ next }
  
  # else save coordinates
  WWTP[as.character(id),"XCOORD"] <- st_coordinates(WWTP.outlet)[wwtp.ind,1]
  WWTP[as.character(id),"YCOORD"] <- st_coordinates(WWTP.outlet)[wwtp.ind,2]
}

# change CRS
WWTP[,"XCOORDMOD"] <- WWTP[,"XCOORD"] + 2000000
WWTP[,"YCOORDMOD"] <- WWTP[,"YCOORD"] + 1000000

### for missing coordinates, use address and ggmap

# loop over WWTP
for(id in WWTP.name){
  
  # if WWTP coordinates are known, skip
  if(!(is.na(WWTP[as.character(id),"XCOORDMOD"]) | is.na(WWTP[as.character(id),"YCOORDMOD"]))){ next }
  
  # find line
  ind <- which(All.WWTPs[,"ARANR"] == id)
  
  location <- c(drop(as.matrix(All.WWTPs[ind,c("Strasse", "Postleitzahl", "Ort")])),"Switzerland")
  if(any(is.na(location[2:3]))){
    stop("Address missing or partly missing")
  }
  
  # create a string with the physical address
  address <- paste(location[!is.na(location)],collapse=" ")
  
  # obtain location from openstreetmap
  url.begin <- "http://nominatim.openstreetmap.org/search/"
  url.end <- "?format=json&addressdetails=0&limit=1"
  geocode.res <- fromJSON( paste0(url.begin,gsub('\\s+', '\\%20', address),url.end) )
  
  # retry without the road name if the first try did not work
  if(is.list(geocode.res) & length(geocode.res) == 0){
    # wait one second (OSM queries for free if only one per second)
    Sys.sleep(1)
    
    address <- paste(location[2:4],collapse=" ")
    geocode.res <- fromJSON( paste0(url.begin,gsub('\\s+', '\\%20', address),url.end) )
    
    # if the second try was not enough, try without postal code
    if(is.list(geocode.res) & length(geocode.res) == 0){
      # wait one second (OSM queries for free if only one per second)
      Sys.sleep(1)
      
      address <- paste(location[3:4],collapse=" ")
      geocode.res <- fromJSON( paste0(url.begin,gsub('\\s+', '\\%20', address),url.end) )
      
      # if third try was not enough, give error message
      if(is.list(geocode.res) & length(geocode.res) == 0){
        stop("Could not find a corresponding geolocation")
      }
    }
  }
  
  cat("Location of '", address, "' obtained\n")
  
  # change from lon/lat to swiss coordinates
  d <- data.frame(lon=as.numeric(geocode.res$lon), lat=as.numeric(geocode.res$lat))
  coordinates(d) <- c("lon", "lat")
  proj4string(d) <- CRS("+init=epsg:4326") # WGS 84
  CRS.new <- CRS("+init=epsg:2056") #LV1903+
  d.ch1903 <- spTransform(d, CRS.new)
  
  # save coordinates
  WWTP[as.character(id),"XCOORDMOD"] <- d.ch1903@coords[1]
  WWTP[as.character(id),"YCOORDMOD"] <- d.ch1903@coords[2]
  
  # wait one second (OSM queries for free if only one per second)
  Sys.sleep(1)
}



### import dataset for the treatment type
category  <- as.matrix(read.xlsx("VSA_Kennzahlen.xlsx", sheetIndex = 1))

# attribute treatment type
for(id in WWTP.name){
  
  # find corresponding WWTP in category
  wwtp.ind <- which(category[,"ARANR"] == id)
  
  # if WWTP available in dataset, include it
  if(length(wwtp.ind) != 0){ 
    WWTP[as.character(id),"CAT"] <- category[wwtp.ind,"Kat_ARA"]
  }
  
}

# translate to: 0 means tertiary treatment, 1 means secondary treatment
# original classification in file:
# 0: unknown
# 1: mechanisch-biologisch
# 2: mechanisch-biologisch mit P-Elimination
# 3: mechanisch-biologisch mit P-Elimination und Filtration
# 4: mechanisch-biologisch mit Nitrifikation
# 5: mechanisch-biologisch mit P-Elimination und Nitrifikation
# 6: mechanisch-biologisch mit P-Elimination, Filtration und Nitrifikation
# 7: mechanisch-biologisch mit Nitrifikation und Denitrifikation
# 8: mechanisch-biologisch mit P-Elimination, Nitrifikation und Denitrifikation
# 9: mechanisch-biologisch mit P-Elimination, Filtration, Nitrifikation und Denitrif.
# 10: mechanisch-biologisch mit P-Elimination, Filtration, Nitrifikation, Denitrifikation und weitergehender Reinigung (z.B. Membran, Ozon, UV)

# first remove unknown
WWTP[(WWTP[,"CAT"] == 0),"CAT"] <- NA

# then attribute "0" to categories which are known to be tertiary
WWTP[(WWTP[,"CAT"] != 1 & !is.na(WWTP[,"CAT"])), "CAT"] <- 0

# attribute "1" to all other
WWTP[(WWTP[,"CAT"] == 1 | is.na(WWTP[,"CAT"])), "CAT"] <- 1


### save data in right format

Data <- list()

# for flows from secondary treatment
Data[["WWTPsec"]] <- WWTP[,c("XCOORDMOD", "YCOORDMOD", "VOL")]
colnames(Data[["WWTPsec"]]) <- c("XCOORD", "YCOORD", "DATA")
Data[["WWTPsec"]][,"DATA"] <- WWTP[,"CAT"]*WWTP[,"VOL"]

# for flows from tertiary treatment
Data[["WWTPter"]] <- WWTP[,c("XCOORDMOD", "YCOORDMOD", "VOL")]
colnames(Data[["WWTPter"]]) <- c("XCOORD", "YCOORD", "DATA")
Data[["WWTPter"]][,"DATA"] <- (1-WWTP[,"CAT"])*WWTP[,"VOL"]


##### CSO #########################################################################################

# second set of data obtained from Lena Mutzner
DATA1 <- read_sf(dsn = "CSO", layer = "Output_Gemeindegebiet2011")

# first set of data obtained from Lena Mutzner
load("run.DSFA.res_final.rdat")
load("pp.NAME_10min_final.rdat")
DATA2 <- matrix(run.Qr.res, nrow = length(pp.NAME), byrow = FALSE)
rownames(DATA2) <- pp.NAME
rm(run.Qr.res,pp.NAME)

## combine into a matrix
# first the volume with municipality number
DATA3 <- t(DATA2[c("BFS","Ueberlaufdauer, h", "Volumen zur ARA inkl. TWA, m3"),])

# calculate the volume of raw wastewater discharged through the CSO (in m3 per year)
CSO <- cbind(DATA3[,"BFS"],
             (DATA3[,"Volumen zur ARA inkl. TWA, m3"]*
                DATA3[,"Ueberlaufdauer, h"]/(24*365)))
rownames(CSO) <- CSO[,1]
colnames(CSO) <- c("BFS", "VolumeDischarged")

# add location of outlet
xy <- cbind(st_coordinates(DATA1),DATA1$BFS_Gemein)
rownames(xy) <- DATA1$BFS_Gemein

# combine both datasets
CSO <- cbind(CSO,xy[as.character(CSO[,"BFS"]),])
Data[["CSO"]] <- CSO[,c("X", "Y", "VolumeDischarged")]

# change CRS
Data[["CSO"]][,"X"] <- CSO[,"X"] + 2000000
Data[["CSO"]][,"Y"] <- CSO[,"Y"] + 1000000
colnames(Data[["CSO"]]) = c("XCOORD", "YCOORD", "DATA")


##### EXPORT ######################################################################################

# merge all three dataframes
df1 <- as.data.frame(Data[["WWTPsec"]])
df2 <- as.data.frame(Data[["WWTPter"]])
df3 <- as.data.frame(Data[["CSO"]])
names(df1)[3] <- "WWTPsec"
names(df2)[3] <- "WWTPter"
names(df3)[3] <- "CSO"
df1["ID"] <- paste0(round(df1$XCOORD),"-",round(df1$YCOORD))
df2["ID"] <- paste0(round(df2$XCOORD),"-",round(df2$YCOORD))
df3["ID"] <- paste0(round(df3$XCOORD),"-",round(df3$YCOORD))

all.df <- merge(df3,merge(df1, df2, by="ID", all = T), by="ID", all = T)

# check that coordinates were correct
stopifnot(unique(all.df$XCOORD.x - all.df$XCOORD.y) %in% c(0,NA))

final.df <- data.frame(XCOORD  = as.numeric(sapply(strsplit(all.df$ID,"-"),function(x) x[1])),
                       YCOORD  = as.numeric(sapply(strsplit(all.df$ID,"-"),function(x) x[2])),
                       WWTPsec = all.df$WWTPsec,
                       WWTPter = all.df$WWTPter,
                       CSO     = all.df$CSO)

# save as sf object
Data.sf <- st_as_sf(final.df, coords = c("XCOORD","YCOORD"))

save(Data.sf, file = "Data_Modified/WWTP_CSO_sf.Rdata")
