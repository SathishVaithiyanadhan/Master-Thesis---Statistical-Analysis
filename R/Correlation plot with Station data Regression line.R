#Mean plot for the 1 day dateset at the end of this R document.
# To Identify the Study Area (AOI)

library(raster)
library(ncdf4)
library(tidyverse)
library(cowplot)
library(rlang)
library(fields)
library(maps)
library(PerformanceAnalytics)
# Northern Italy Extent: 6.25, 15, 43.95, 47.25 
# Polyphemus Extent: -1.0625, 16.9375, 43.96875, 51.96875
#paris extent:0.922852,48.022998,3.944092,49.567978.
# import netcdf as raster
#Polyphemus Regridded
PNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/POLYPHEMUS/NO2/*.nc"),raster::brick,level=1)

CNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/CAMS/NO2/*.nc"),raster::brick,level=1)

SNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/NO2/*.nc"),raster::brick,level=1)


PNO2names <- list.files("E:/Master Thesis/POLYPHEMUS/Domain2/NO2 Monthly data Regridded/",pattern="*.nc",full.names=FALSE)
PNO2names



####Correlation plot
#memory.limit(size=600000000)
PNO2.df<-list(PNO2)
for (i in 1:length(PNO2)) {
  
  PNO2.df[[i]] = raster::as.data.frame(PNO2[[i]],na.rm=TRUE)
  #subset(PNO2.df[[i]], select= -(x:y))
  
}


CNO2.df<-list(CNO2)
for (i in 1:length(CNO2)) {
  CNO2.df[[i]] = raster::as.data.frame(CNO2[[i]],na.rm=TRUE)
}


SNO2.df<-list(SNO2)
for (i in 1:length(CNO2)) {
  SNO2.df[[i]] = raster::as.data.frame(SNO2[[i]],na.rm=TRUE)
}

#Mean
MeanPNO2<-list(PNO2.df)
for(i in 1:length(PNO2.df)){
  MeanPNO2[[i]]<-as.array(colMeans(PNO2.df[[i]])) # calculating Column means
}
MeanPNO2[[31]]


MeanCNO2<-list(CNO2.df)
for(i in 1:length(CNO2.df)){
  MeanCNO2[[i]]<-as.array(colMeans(CNO2.df[[i]]))
}
class(MeanCNO2[[1]])
MeanCNO2[[1]]

MeanSNO2<-list(SNO2.df)
for(i in 1:length(SNO2.df)){
  MeanSNO2[[i]]<-as.array(colMeans(SNO2.df[[i]]))
}
#

corr_poly_NO2<-list(MeanPNO2) 
for (i in 1:length(MeanPNO2)) {
  corr_poly_NO2[[i]]<-data.frame(MeanPNO2[[i]])
  colnames(corr_poly_NO2[[i]])[1]<-"Polyphemus"
  
}
corr_poly_NO2[[31]]

corr_CAMS_NO2<-list(MeanCNO2) 
for (i in 1:length(MeanCNO2)) {
  corr_CAMS_NO2[[i]]<-data.frame(MeanCNO2[[i]]) # naming columns
  colnames(corr_CAMS_NO2[[i]])[1]<-"CAMS"
  
}
corr_CAMS_NO2[[30]]


corr_Sta_NO2<-list(MeanSNO2) 
for (i in 1:length(MeanSNO2)) {
  corr_Sta_NO2[[i]]<-data.frame(MeanSNO2[[i]])
  colnames(corr_Sta_NO2[[i]])[1]<-"Station"
  
}
corr_Sta_NO2[[30]]
#

binding<-list(corr_poly_NO2,corr_Sta_NO2) 

for(i in 1:length(corr_poly_NO2)){
  for (j in 1:length(corr_poly_NO2)) { # generating DF with column means
    binding[[j]]<-cbind(corr_poly_NO2[[j]],corr_Sta_NO2[[j]]) 
  }
  
}
binding[[1]]
binding2<-list(corr_CAMS_NO2,corr_Sta_NO2) 

for(i in 1:length(corr_CAMS_NO2)){
  for (j in 1:length(corr_CAMS_NO2)) {
    binding2[[j]]<-cbind(corr_CAMS_NO2[[j]],corr_Sta_NO2[[j]]) 
  }
  
}
#


######
library("ggpubr")
corr_plot_Poly<-list(binding) 
for (i in 1:31) { # calculating monthly Correlation between Poly and station data
  corr_plot_Poly[[i]]<-ggscatter(binding[[i]], x = "Station", y = "Polyphemus", 
                                  add = "reg.line", add.params = list(color = "blue"), conf.int = TRUE, 
                                  cor.coef = TRUE, cor.coeff.args = list(method = "pearson",  label.sep = "\n"),title = paste0("Correlation_Station Vs Polyphemus_",PNO2names[[i]]),
                                  xlab = "NO2_Station (µg/m3)", ylab = "NO2_Polyphemus (µg/m3)")
  
}
corr_plot_CAMS<-list(binding2)
for (i in 1:31) { # calculating monthly Correlation between CAMS and station data
  corr_plot_CAMS[[i]]<-ggscatter(binding2[[i]], x = "Station", y = "CAMS", 
                                 add = "reg.line", add.params = list(color = "red"), conf.int = TRUE, 
                                 cor.coef = TRUE, cor.coeff.args = list(method = "pearson",  label.sep = "\n"),title = paste0("Correlation_Station Vs CAMS_",PNO2names[[i]]),
                                 xlab = "NO2_Station (µg/m3)", ylab = "NO2_CAMS (µg/m3)")
  
}

library(ggplot2)
library(gridExtra)
library(grid)
for (i in 1:31) { # arranging both the plots together in same page.
  
  file_name = paste("Correlation with Regression line_",PNO2names[[i]], ".png", sep = "")
  tiff(file_name)
 grid.arrange(corr_plot_Poly[[i]],corr_plot_CAMS[[i]])
  dev.off()
}

###############################


