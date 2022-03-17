#Mean plot for the 1 day dateset at the end of this R document.
# To Identify the Study Area (AOI)
memory.limit(size=600000000)
library(raster)
library(ncdf4)
library(tidyverse)
library(cowplot)
library(rlang)
library(fields)

# loading NetCDF files
NO2_Station<-lapply(Sys.glob("E:/Master Thesis/Station Data/NO2/*.nc"),raster::brick,level=1)
NO2_CAMS<-lapply(Sys.glob("E:/Master Thesis/CAMS reanalysis/NO2/*.nc"),raster::brick,level=1)
NO2_Poly<-lapply(Sys.glob("E:/Master Thesis/POLYPHEMUS/Domain2/NO2 Monthly data Regridded/*.nc"),raster::brick,level=1)

myFiles <- list.files("E:/Master Thesis/POLYPHEMUS/Domain2/NO2 Monthly data Regridded/",pattern="*.nc",full.names=FALSE)
myFiles


for (i in 1:length(NO2_CAMS)) { # Changing the NA locations from Station datasets in every time step to CAMS datasets. 
  #Now both the datasets have same pixels as in station data for better data comparison 
  NO2_CAMS[[i]][][which(is.na(NO2_Station[[i]][]))] = NA #very important
  writeRaster(NO2_CAMS[[i]], file=paste0("CAMS_Station_",myFiles[[i]]),varname="PM10",overwrite =TRUE )
}

for (i in 1:length(NO2_Poly)) {# Changing the NA locations from Station datasets in every time step to Polyphemus datasets. 
  #Now both the datasets have same pixels as in station data for better data comparison 
  NO2_Poly[[i]][][which(is.na(NO2_Station[[i]][]))] = NA #very important
  writeRaster(NO2_Poly[[i]], file=paste0("Poly_Station_",myFiles[[i]]),varname="PM10",overwrite =TRUE )
}











