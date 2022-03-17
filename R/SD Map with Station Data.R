#Mean plot for the 1 day dateset at the end of this R document.
# To Identify the Study Area (AOI)
#memory.limit(size=600000000)
library(raster)
library(ncdf4)
library(tidyverse)
library(cowplot)
library(rlang)
library(fields)
library(ggplot2)
library("maps")
#CNO2<-lapply(Sys.glob("~/+KA/ufsworld/hawking/hawking1/vait_sa/Station Data/CAMS/NO2/*.nc"),raster::brick,level=1)
#PNO2<-lapply(Sys.glob("~/+KA/ufsworld/hawking/hawking1/vait_sa/Station Data/POLYPHEMUS/NO2/*.nc"),raster::brick,level=1)
#SNO2<-lapply(Sys.glob("~/+KA/ufsworld/hawking/hawking1/vait_sa/Station Data/NO2/*.nc"),raster::brick,level=1)

#loading datasets 
PNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/POLYPHEMUS/NO2/*.nc"),raster::brick,level=1)

CNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/CAMS/NO2/*.nc"),raster::brick,level=1)

SNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/NO2/*.nc"),raster::brick,level=1)

dim(PNO2[[5]])
dim(CNO2[[5]])
dim(SNO2[[5]])


Standard_Deviation <- function(x){ sd(x,na.rm = TRUE)} # creating function to calculate SD maps.
#calculating SD

Poly_NO2_SD <- list(PNO2) 

for (i in 1:length(PNO2)) {
  Poly_NO2_SD[[i]]<- calc(PNO2[[i]],Standard_Deviation) #creating a loop for calculating mean for each pixel of all the dataset loaded.
}

CAMS_NO2_SD <- list(CNO2)  

for (i in 1:length(CNO2)) {
  CAMS_NO2_SD[[i]]<- calc(CNO2[[i]],Standard_Deviation) 
}

Station_NO2_SD <- list(SNO2)  

for (i in 1:length(SNO2)) {
  Station_NO2_SD[[i]]<- calc(SNO2[[i]],Standard_Deviation) 
}

PNO2names <- list.files("E:/Master Thesis/Station Data/NO2/",pattern="*.nc",full.names=FALSE)
PNO2names
#Maps
zlim<-list(Poly_NO2_SD)
for (i in 1:31) {#plots with common color bars.
  
  mypath <- file.path("~","+KA","ufsworld","hawking","hawking1","vait_sa","Codes and result",paste("Standard Deviation_", PNO2names[[i]], ".png", sep = ""))
  #par(mar=c(0,0,0,0))
  png(file=mypath,width= 700,# defining jpeg quality 1224x571
      height    = 571)
  
  par(mfrow=c(2,2) )
  
  pal <- colorRampPalette(c("red","orange","pink","magenta","honeydew","aquamarine3","purple","blue","cyan","mediumorchid2","green","yellow"))
  
  zlim[[i]] <- range(values(Poly_NO2_SD[[i]]))
  PNO2[[i]]<-raster::plot(Poly_NO2_SD[[i]], main =  (paste0("Polyphemus_Std. Deviation_",PNO2names[[i]])), xlab = "lon", ylab="lat",bty="n",zlim=zlim[[i]],legend=FALSE,col=pal(41),asp=0)
  
  map(add=TRUE, col="black") # country lines specifications and permission to define in  the above image plot
  # Create points of interest
  place <- c("Paris", "Zurich", "Munich", "Brussels", "Milan", "Cologne","Frankfurt","London","Prague","Vienna","Essen") #Essen: 51.450833, 7.013056
  p.lon <- c(2.346954, 8.55,  11.566667, 4.35,9.19, 6.952778,8.683333,-0.1275,14.416667,16.366667,7.013056)
  p.lat <- c(48.857035, 47.366667, 48.133333, 50.85, 45.466944, 50.936389,50.116667,51.507222,50.083333,48.2,51.450833)
  # Add points to map
  points(p.lon, p.lat, pch=20, col="black", bg="blue", cex=1)
  # Add place names to map
  text(p.lon, p.lat, labels=place, cex=0.60, pos=1) 
  
  CNO2[[i]]<-raster::plot(CAMS_NO2_SD[[i]], main = (paste0("CAMS_Std. Deviation_",PNO2names[[i]])), xlab = "lon", ylab="lat",bty="n",zlim=zlim[[i]], legend=FALSE,col=pal(41),asp=0)
  
  map(add=TRUE, col="black") # country lines specifications and permission to define in  the above image plot
  # Create points of interest
  place <- c("Paris", "Zurich", "Munich", "Brussels", "Milan", "Cologne","Frankfurt","London","Prague","Vienna","Essen") #Essen: 51.450833, 7.013056
  p.lon <- c(2.346954, 8.55,  11.566667, 4.35,9.19, 6.952778,8.683333,-0.1275,14.416667,16.366667,7.013056)
  p.lat <- c(48.857035, 47.366667, 48.133333, 50.85, 45.466944, 50.936389,50.116667,51.507222,50.083333,48.2,51.450833)
  # Add points to map
  points(p.lon, p.lat, pch=20, col="black", bg="blue", cex=1)
  # Add place names to map
  text(p.lon, p.lat, labels=place, cex=0.60, pos=1) 
  
  SNO2[[i]]<-raster::plot(Station_NO2_SD[[i]], main = (paste0("Station_Std. Deviation_",PNO2names[[i]])), xlab = "lon", ylab="lat",bty="n",zlim=zlim[[i]], legend=FALSE,col=pal(41),asp=0)
  
  map(add=TRUE, col="black") # country lines specifications and permission to define in  the above image plot
  # Create points of interest
  place <- c("Paris", "Zurich", "Munich", "Brussels", "Milan", "Cologne","Frankfurt","London","Prague","Vienna","Essen") #Essen: 51.450833, 7.013056
  p.lon <- c(2.346954, 8.55,  11.566667, 4.35,9.19, 6.952778,8.683333,-0.1275,14.416667,16.366667,7.013056)
  p.lat <- c(48.857035, 47.366667, 48.133333, 50.85, 45.466944, 50.936389,50.116667,51.507222,50.083333,48.2,51.450833)
  # Add points to map
  points(p.lon, p.lat, pch=20, col="black", bg="blue", cex=1)
  # Add place names to map
  text(p.lon, p.lat, labels=place, cex=0.60, pos=1) 
  plot(Poly_NO2_SD[[i]], zlim=zlim[[i]],  legend.only=TRUE,inset = c(0, -0.3),legend.width=2, legend.shrink=0.50,col=pal(41),legend.args = list(text='NO2 (µg/m3)',pch=16,font=1.5, line=2.5, cex=0.8)) 
  #breaks=cuts
  dev.off()
}
