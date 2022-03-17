## Regridding several Polyphemus data in a loop.
memory.limit(size=600000000)
library(ncdf4)
library(fields)
library(raster)
myFiles <- list.files("F:/Master Thesis/POLYPHEMUS/Domain2/PM10 Monthly data Regridded/",pattern="*.nc",full.names=FALSE)
myFiles #loading file names
zyx<-Sys.glob("F:/Master Thesis/POLYPHEMUS/Domain2/PM10 Monthly data Regridded/*.nc") 
zyx # loading files
zyxw<-lapply(zyx,raster::brick,level=1)
zyxw # extracting data from level 1
PM10CAMS<-brick("D:/MUENSTER_UNIVERSITY/SEMESTER_4/DLR/NetCDF data and tutorial/Reanalysis data/ENSa.2018.PM10_720_1439.yearlyrea.nc",level=1) #loading source for target grid data
proj4string(PM10CAMS) <-CRS("+proj=longlat +datum=WGS84 +no_defs") # mentioning projection required
AOI<- extent(-1.0625, 16.9375, 43.96875, 51.96875)# Polyphemus area extent
PM10CAMS<-crop(PM10CAMS, AOI) #crop the cams data wrt to the area of the Polyphemus
PM10CAMS
dim(PM10CAMS) 
names(PM10CAMS)
library(terra)
# Northern Italy Extent: 6.262207,43.992815,13.930664,47.219568 
rs <- list(PM10CAMS)
rs
for (i in 1:length(zyxw)) {
  rs[[i]] <- resample(zyxw[[i]], PM10CAMS, method="ngb") # performing regridding or reprojection using nearest neighbour interpolation
  writeRaster(rs[[i]], file=paste0("Regridded_",myFiles[[i]]),varname="PM10",overwrite =TRUE ) # exporting the regridded data as new netcdf files.
}


