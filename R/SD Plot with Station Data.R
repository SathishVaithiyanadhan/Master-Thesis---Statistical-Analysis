
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
#Plots
# coverting NetCDF to DF.

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
#Calculating SD
PNO2_SD<-list(PNO2.df)

for (i in 1:length(PNO2)) {
  PNO2_SD[[i]]<-sd(as.matrix(PNO2.df[[i]]))
  
}

CNO2_SD<-list(CNO2.df)

for (i in 1:length(CNO2)) {
  CNO2_SD[[i]]<-sd(as.matrix(CNO2.df[[i]]))
  
}

SNO2_SD<-list(SNO2.df)

for (i in 1:length(SNO2)) {
  SNO2_SD[[i]]<-sd(as.matrix(SNO2.df[[i]]))
  
}
PNO2_SD
a<-unlist(PNO2_SD)
b<-unlist(CNO2_SD)
c<-unlist(SNO2_SD)
a
b
c
month <- seq(as.Date("2016-06-01"), 
             as.Date("2018-12-31"), 
             by = "1 month")
month
P_NO2.df<-data.frame(month <- month,a<-a[c(1:31)]) 
C_NO2.df<-data.frame(month <- month,b<-b[c(1:31)]) 
S_NO2.df<-data.frame(month <- month,c<-c[c(1:31)]) 
P_NO2.df
png(file="E:/Master Thesis/NO2 SD PLOT June 2016 - Dec 2018.png",width = 1224,height = 571)

colors<-c("Polyphemus"="green","CAMS"="red","Station"="blue")
ggplot() + 
  geom_line(data=P_NO2.df, aes(x=month, y=a,color="Polyphemus" ) ) + 
  geom_line(data=C_NO2.df, aes(x=month, y=b,color="CAMS")  )+
  geom_line(data=S_NO2.df, aes(x=month, y=c,color="Station")  )+
  scale_x_date(date_breaks="1 month", date_labels="%b-%y") +
  labs(title="Standard deviation NO2 (Polyphemus, CAMS Reanalysis, Station Data) June 2016 - Dec 2018 ",
       x="Month",y="Concentration (µg/m3)",colour="Model:")+theme(axis.text=element_text(angle=90,hjust=1,vjust=0.5,size=20))+theme(legend.position = "bottom" )+scale_color_manual(values = colors)+theme(axis.title.x= element_text(size = 20,margin = margin(t =50)))+
  theme(legend.text = element_text(size = 20))+theme(legend.title = element_text(size = 20))+theme(plot.title = element_text(size = 20))+theme(axis.title.y= element_text(size = 20))
dev.off()