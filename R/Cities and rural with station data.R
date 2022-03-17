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

# Polyphemus Extent: -1.0625, 16.9375, 43.96875, 51.96875

# import netcdf as raster
Polyphemus<-lapply(Sys.glob("E:/Master Thesis/Station Data/POLYPHEMUS/NO2/*.nc"),raster::brick,level=1)

CAMS<-lapply(Sys.glob("E:/Master Thesis/Station Data/CAMS/NO2/*.nc"),raster::brick,level=1)

Station<-lapply(Sys.glob("E:/Master Thesis/Station Data/NO2/*.nc"),raster::brick,level=1)


AOI_paris<- extent(1.35, 3.75, 48.02, 49.25)# Paris
AOI_Milan<- extent(8.73,9.64,45.25,45.65)#Milan
AOI_Cologne<- extent(6.38,7.81,50.68,51.54) # cologne, Bonn, Dusseldorf
AOI_Poly_west_rural<-extent(0.61,6.08,44.46,47.53) # western rural Polyphemus domain
AOI_Poly_east_rural<-extent(11.84,15.95,46.72,51.72) # eastern rural Polyphemus domain

for(i in 1:length(Polyphemus)){ # cropping data for cologne regions
  Polyphemus[[i]]<-crop(Polyphemus[[i]],AOI_Cologne)
}

for(i in 1:length(CAMS)){
  CAMS[[i]]<-crop(CAMS[[i]],AOI_Cologne)
}

for(i in 1:length(Station)){
  Station[[i]]<-crop(Station[[i]],AOI_Cologne)
}


dim(Polyphemus[[4]])
dim(CAMS[[4]])
CAMS[[4]]
Polyphemus[[4]]

#plot
# compute for each grid cell across all raster layers the Mean for result as graph.
Polyphemus.df<-list(Polyphemus)
for (i in 1:length(Polyphemus)) {
  
  Polyphemus.df[[i]] = raster::as.data.frame(Polyphemus[[i]])
  
}
CAMS.df<-list(CAMS)
for (i in 1:length(CAMS)) {
  CAMS.df[[i]] = raster::as.data.frame(CAMS[[i]])
}

Station.df<-list(Station)
for (i in 1:length(Station)) {
  Station.df[[i]] = raster::as.data.frame(Station[[i]])
}
Station.df[[7]]

for (i in 1:length(Polyphemus)) {
  Polyphemus.df[[i]]<-as.matrix(Polyphemus.df[[i]])
  
}
for (i in 1:length(CAMS)) {
  CAMS.df[[i]]<-as.matrix(CAMS.df[[i]])
  
}


for (i in 1:length(Station)) {
  Station.df[[i]]<-as.matrix(Station.df[[i]])
  
}
Station.df[[5]]
#Calculating mean
Polyphemus_Mean<-list(Polyphemus.df)

for (i in 1:length(Polyphemus)) {
  Polyphemus_Mean[[i]]<-mean(Polyphemus.df[[i]],na.rm=TRUE)
  
}

CAMS_Mean<-list(CAMS.df)

for (i in 1:length(CAMS)) {
  CAMS_Mean[[i]]<-mean(CAMS.df[[i]],na.rm=TRUE)
  
}

Station_Mean<-list(Station.df)

for (i in 1:length(Station)) {
  Station_Mean[[i]]<-mean(Station.df[[i]],na.rm=TRUE)
  
}

a<-unlist(Polyphemus_Mean)
b<-unlist(CAMS_Mean)
c<-unlist(Station_Mean)
a
month <- seq(as.Date("2016-06-01"), #generating month sequence
             as.Date("2018-12-31"), 
             by = "1 month")
month
P.df<-data.frame(month <- month,a<-a[c(1:31)]) # df with month and mean together
C.df<-data.frame(month <- month,b<-b[c(1:31)]) 
S.df<-data.frame(month <- month,c<-c[c(1:31)]) 
png(file="E:/Master Thesis/Köln, Düsseldorf, Essen, Bonn NO2 Mean 2016 - 2018.png",width = 1224,height = 571)
colors<-c("Polyphemus"="green","CAMS"="red","Station"="blue")
ggplot() + 
  geom_line(data=P.df, aes(x=month, y=a,color="Polyphemus" ) ) + 
  geom_line(data=C.df, aes(x=month, y=b,color="CAMS")  )+
  geom_line(data=S.df, aes(x=month, y=c,color="Station")  )+
  scale_x_date(date_breaks="1 month", date_labels="%b-%y") +
  labs(title="Köln, Düsseldorf, Essen, Bonn NO2 Mean (Polyphemus, CAMS Reanalysis, Station Data) June 2016 - Dec 2018 ",
       x="Month",y="Concentration (µg/m3)",colour="Model:")+theme(axis.text=element_text(angle=90,hjust=1,vjust=0.5,size=20))+theme(legend.position = "bottom" )+scale_color_manual(values = colors)+theme(axis.title.x= element_text(size = 20,margin = margin(t =50)))+
  theme(legend.text = element_text(size = 20))+theme(legend.title = element_text(size = 20))+theme(plot.title = element_text(size = 20))+theme(axis.title.y= element_text(size = 20))
dev.off()
############################################

