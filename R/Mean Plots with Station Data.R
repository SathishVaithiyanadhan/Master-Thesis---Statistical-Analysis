#Mean plot for the 1 day dateset at the end of this R document.
# To Identify the Study Area (AOI)
memory.limit(size=600000000)
library(raster)
library(ncdf4)
library(tidyverse)
library(cowplot)
library(rlang)
library(fields)
library(ggplot2)
library("maps")
# loading NetCDF files

PNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/POLYPHEMUS/NO2/*.nc"),raster::brick,level=1)

CNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/CAMS/NO2/*.nc"),raster::brick,level=1)

SNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/NO2/*.nc"),raster::brick,level=1)

#Plots
#converting NetCDF to DF.
PNO2.df<-list(PNO2)
for (i in 1:length(PNO2)) {
  
  PNO2.df[[i]] = raster::as.data.frame(PNO2[[i]], na.rm=TRUE)
  #subset(PNO2.df[[i]], select= -(x:y))
  
}

CNO2.df<-list(CNO2)
for (i in 1:length(CNO2)) {
  CNO2.df[[i]] = raster::as.data.frame(CNO2[[i]], na.rm=TRUE)
}

SNO2.df<-list(SNO2)
for (i in 1:length(SNO2)) {
  SNO2.df[[i]] = raster::as.data.frame(SNO2[[i]], na.rm=TRUE)
}
#Calculating mean
PNO2_Mean<-list(PNO2.df)

for (i in 1:length(PNO2)) {
  PNO2_Mean[[i]]<-mean(as.matrix(PNO2.df[[i]]),na.rm=TRUE)
  
}

CNO2_Mean<-list(CNO2.df)

for (i in 1:length(CNO2)) {
  CNO2_Mean[[i]]<-mean(as.matrix(CNO2.df[[i]]),na.rm=TRUE)
  
}

SNO2_Mean<-list(SNO2.df)

for (i in 1:length(SNO2)) {
  SNO2_Mean[[i]]<-mean(as.matrix(SNO2.df[[i]]),na.rm=TRUE)
  
}
PNO2_Mean
a<-unlist(PNO2_Mean)
b<-unlist(CNO2_Mean)
c<-unlist(SNO2_Mean)
a
b
c
month <- seq(as.Date("2016-06-01"), # generating month sequence.
             as.Date("2018-12-31"), 
             by = "1 month")
month
P_NO2.df<-data.frame(month <- month,a<-a[c(1:31)]) 
C_NO2.df<-data.frame(month <- month,b<-b[c(1:31)]) 
S_NO2.df<-data.frame(month <- month,c<-c[c(1:31)]) 
P_NO2.df
S_NO2.df
S_Diff_P<-c-a
S_Diff_P
S_Diff_C<-c-b
S_Diff_C

# plot

png(file="E:/Master Thesis/Mean NO2 2016 - 2018.png",width = 1224,height = 571)
colors<-c("Polyphemus"="green","CAMS"="red","Station"="blue")
ggplot() + 
  geom_line(data=P_NO2.df, aes(x=month, y=a,color="Polyphemus" ) ) + 
  geom_line(data=C_NO2.df, aes(x=month, y=b,color="CAMS")  )+
  geom_line(data=S_NO2.df, aes(x=month, y=c,color="Station")  )+
  scale_x_date(date_breaks="1 month", date_labels="%b-%y") +
  labs(title="Mean NO2 (Polyphemus, CAMS Reanalysis, Station Data) June 2016 - Dec 2018 ",
       x="Month",y="Concentration (µg/m3)",colour="Model:")+theme(axis.text=element_text(angle=90,hjust=1,vjust=0.5,size=20))+theme(legend.position = "bottom" )+scale_color_manual(values = colors)+theme(axis.title.x= element_text(size = 20,margin = margin(t =50)))+
  theme(legend.text = element_text(size = 20))+theme(legend.title = element_text(size = 20))+theme(plot.title = element_text(size = 20))+theme(axis.title.y= element_text(size = 20))
dev.off()
