#Mean plot for the 1 day dateset at the end of this R document.
# To Identify the Study Area (AOI)
memory.limit(size=600000000)
library(raster)
library(ncdf4)
library(tidyverse)
library(cowplot)
library(rlang)
library(fields)
library(maps)

# import netcdf as raster
#Polyphemus Regridded
#PNO2<-lapply(Sys.glob("~/+KA/ufsworld/hawking/hawking1/vait_sa/Station Data/POLYPHEMUS/NO2/*.nc"),raster::brick,level=1)
#CNO2<-lapply(Sys.glob("~/+KA/ufsworld/hawking/hawking1/vait_sa/Station Data/CAMS/NO2/*.nc"),raster::brick,level=1)
#SNO2<-lapply(Sys.glob("~/+KA/ufsworld/hawking/hawking1/vait_sa/Station Data/NO2/*.nc"),raster::brick,level=1)

PNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/POLYPHEMUS/NO2/*.nc"),raster::brick,level=1)

CNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/CAMS/NO2/*.nc"),raster::brick,level=1)

SNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/NO2/*.nc"),raster::brick,level=1)



PNO2names <- list.files("E:/Master Thesis/Station Data/POLYPHEMUS/NO2/",pattern="*.nc",full.names=FALSE)
PNO2names

#Correlation_Coefficient <- function(x){ corLocal(x,y,na.rm = TRUE)}
#NO2 
corr_NO2_PS<-list(PNO2,SNO2)

for(i in 1:length(PNO2)){
  for (j in 1:length(SNO2)) {
    corr_NO2_PS[[j]]<-corLocal(PNO2[[j]],SNO2[[j]],test=TRUE) # calculating Correlationa and P-value
    #names(corr_NO2_PS[[j]])<-c("Correlation Polyphemus VS Station Data","P-value")
  }
  
}
corr_NO2_CS<-list(CNO2,SNO2)

for(i in 1:length(CNO2)){
  for (j in 1:length(SNO2)) {
    corr_NO2_CS[[j]]<-corLocal(CNO2[[j]],SNO2[[j]],test=TRUE) 
    #names(corr_NO2_CS[[j]])<-c("Correlation CAMS VS Station Data","P-value")
  }
  
}

for (i in 1:length(corr_NO2_PS)) {
  names(corr_NO2_PS[[i]])<-c("Correlation Polyphemus VS Station Data","P-value") # defining names for the maps
}
for (i in 1:length(corr_NO2_CS)) {
  names(corr_NO2_CS[[i]])<-c("Correlation CAMS VS Station Data","P-value")
}

Plot_poly<-list(corr_NO2_PS)
Plot_poly_Pvalue<-list(corr_NO2_PS)
Plot_CAMS<-list(corr_NO2_CS)
Plot_CAMS_Pvalue<-list(corr_NO2_CS)
for (i in 1:length(PNO2)) { # Correlation and P-value maps
  mypath <- file.path("E:","Master Thesis",paste("Correlation_", PNO2names[[i]], ".png", sep = ""))
  png(file=mypath,width= 700, # defining jpeg quality 1224x571
      height    = 571)
  par(mfrow=c(2,2))
  Plot_poly[[i]]<-raster::plot(corr_NO2_PS[[i]]$Correlation.Polyphemus.VS.Station.Data,asp=0,main = (paste0("Correlation Poly VS Station_",PNO2names[[i]])), xlab = "lon", ylab="lat",bty="n",legend=TRUE,legend.args = list(text='NO2_Conc (µg/m3)',pch=16,font=1.5, line=2.5, cex=0.8),inset = c(0, -0.3),legend.width=1, legend.shrink=0.50)
  library("maps") #package to add country lines.
  map(add=TRUE, col="black") # country lines specifications and permission to define in  the above image plot
  # Create points of interest
  place <- c("Paris", "Zurich", "Munich", "Brussels", "Milan", "Cologne","Frankfurt","London","Prague","Vienna","Essen") #Essen: 51.450833, 7.013056
  p.lon <- c(2.346954, 8.55,  11.566667, 4.35,9.19, 6.952778,8.683333,-0.1275,14.416667,16.366667,7.013056)
  p.lat <- c(48.857035, 47.366667, 48.133333, 50.85, 45.466944, 50.936389,50.116667,51.507222,50.083333,48.2,51.450833)
  # Add points to map
  points(p.lon, p.lat, pch=20, col="black", bg="blue", cex=1)
  # Add place names to map
  text(p.lon, p.lat, labels=place, cex=0.9, pos=1)
  Plot_poly_Pvalue[[i]]<-raster::plot(corr_NO2_PS[[i]]$P.value,asp=0,main = (paste0("P-Value_",PNO2names[[i]])), xlab = "lon", ylab="lat",bty="n",legend=TRUE,legend.args = list(text='NO2_Conc (µg/m3)',pch=16,font=1.5, line=2.5, cex=0.8),inset = c(0, -0.3),legend.width=1, legend.shrink=0.50)
  
  map(add=TRUE, col="black") # country lines specifications and permission to define in  the above image plot
  # Create points of interest
  place <- c("Paris", "Zurich", "Munich", "Brussels", "Milan", "Cologne","Frankfurt","London","Prague","Vienna","Essen") #Essen: 51.450833, 7.013056
  p.lon <- c(2.346954, 8.55,  11.566667, 4.35,9.19, 6.952778,8.683333,-0.1275,14.416667,16.366667,7.013056)
  p.lat <- c(48.857035, 47.366667, 48.133333, 50.85, 45.466944, 50.936389,50.116667,51.507222,50.083333,48.2,51.450833)
  # Add points to map
  points(p.lon, p.lat, pch=20, col="black", bg="blue", cex=1)
  # Add place names to map
  text(p.lon, p.lat, labels=place, cex=0.9, pos=1)
  
  Plot_CAMS[[i]]<-raster::plot(corr_NO2_CS[[i]]$Correlation.CAMS.VS.Station.Data,asp=0,main = (paste0("Correlation CAMS VS Station_",PNO2names[[i]])), xlab = "lon", ylab="lat",bty="n",legend=TRUE,legend.args = list(text='NO2_Conc (µg/m3)',pch=16,font=1.5, line=2.5, cex=0.8),legend.width=1, legend.shrink=0.50)
  
  map(add=TRUE, col="black") # country lines specifications and permission to define in  the above image plot
  # Create points of interest
  place <- c("Paris", "Zurich", "Munich", "Brussels", "Milan", "Cologne","Frankfurt","London","Prague","Vienna","Essen") #Essen: 51.450833, 7.013056
  p.lon <- c(2.346954, 8.55,  11.566667, 4.35,9.19, 6.952778,8.683333,-0.1275,14.416667,16.366667,7.013056)
  p.lat <- c(48.857035, 47.366667, 48.133333, 50.85, 45.466944, 50.936389,50.116667,51.507222,50.083333,48.2,51.450833)
  # Add points to map
  points(p.lon, p.lat, pch=20, col="black", bg="blue", cex=1)
  # Add place names to map
  text(p.lon, p.lat, labels=place, cex=0.9, pos=1)
  Plot_CAMS_Pvalue[[i]]<-raster::plot(corr_NO2_CS[[i]]$P.value,asp=0,main = (paste0("P-Value_",PNO2names[[i]])), xlab = "lon", ylab="lat",bty="n",legend=TRUE,legend.args = list(text='NO2_Conc (µg/m3)',pch=16,font=1.5, line=2.5, cex=0.8),legend.width=1, legend.shrink=0.50)
  
  map(add=TRUE, col="black") # country lines specifications and permission to define in  the above image plot
  # Create points of interest
  place <- c("Paris", "Zurich", "Munich", "Brussels", "Milan", "Cologne","Frankfurt","London","Prague","Vienna","Essen") #Essen: 51.450833, 7.013056
  p.lon <- c(2.346954, 8.55,  11.566667, 4.35,9.19, 6.952778,8.683333,-0.1275,14.416667,16.366667,7.013056)
  p.lat <- c(48.857035, 47.366667, 48.133333, 50.85, 45.466944, 50.936389,50.116667,51.507222,50.083333,48.2,51.450833)
  # Add points to map
  points(p.lon, p.lat, pch=20, col="black", bg="blue", cex=1)
  # Add place names to map
  text(p.lon, p.lat, labels=place, cex=0.9, pos=1)
  
  dev.off()
  
}


