memory.limit(size=600000000)
library(raster)
library(ncdf4)
library(tidyverse)
library(cowplot)
library(rlang)
library(fields)
library(ggplot2)
library(gridExtra)
library(grid)
library(Kendall)
library(trend)
library(lattice)

#day and night segregated model and station data
# import netcdf as raster 
Polyphemus<-lapply(Sys.glob("E:/Master Thesis/Station Data/day and ni8/am 6 to pm 6/POLYPHEMUS/O3/*.nc"),raster::brick,level=1)

CAMS<-lapply(Sys.glob("E:/Master Thesis/Station Data/day and ni8/am 6 to pm 6/CAMS/O3/*.nc"),raster::brick,level=1)

Station<-lapply(Sys.glob("E:/Master Thesis/Station Data/day and ni8/am 6 to pm 6/STATION/O3/*.nc"),raster::brick,level=1)

#DATAFRAME
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
P.df<-data.frame(month <- month,a<-a[c(1:31)])  # df with month and mean together
C.df<-data.frame(month <- month,b<-b[c(1:31)]) 
S.df<-data.frame(month <- month,c<-c[c(1:31)])

png(file="E:/Master Thesis/O3 Day emissions (am 6 to pm 6) Station, Polyphemus and CAMS 2016 - 2018.png",width = 1224,height = 571)
colors<-c("Polyphemus"="green","CAMS"="red","Station"="blue")
ggplot() + 
  geom_line(data=P.df, aes(x=month, y=a,color="Polyphemus" ) ) + 
  geom_line(data=C.df, aes(x=month, y=b,color="CAMS")  )+
  geom_line(data=S.df, aes(x=month, y=c,color="Station")  )+
  scale_x_date(date_breaks="1 month", date_labels="%b-%y") +
  labs(title="O3 Day emissions (6 AM to 6 PM) Station, Polyphemus and CAMS 2016 - 2018 ",
       x="Month",y="Concentration (µg/m3)",colour="Model:")+theme(axis.text=element_text(angle=90,hjust=1,vjust=0.5,size=20))+theme(legend.position = "bottom" )+scale_color_manual(values = colors)+theme(axis.title.x= element_text(size = 20,margin = margin(t =50)))+
  theme(legend.text = element_text(size = 20))+theme(legend.title = element_text(size = 20))+theme(plot.title = element_text(size = 20))+theme(axis.title.y= element_text(size = 20))
dev.off()