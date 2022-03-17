library(raster)
library(ncdf4)
library(tidyverse)
library(cowplot)
library(rlang)
library(fields)
library(maps)
#loading NetCDF files
Polyphemus<-lapply(Sys.glob("E:/Master Thesis/Station Data/POLYPHEMUS/PM10/*.nc"),raster::brick,level=1)

CAMS<-lapply(Sys.glob("E:/Master Thesis/Station Data/CAMS/PM10/*.nc"),raster::brick,level=1)

Station<-lapply(Sys.glob("E:/Master Thesis/Station Data/PM10/*.nc"),raster::brick,level=1)

Mean_Bias = function(x,y){ # function to calculate Mean bias 
  mean(x-y,na.rm=TRUE)
}


#plot
# NetCDF to DF
Polyphemus.df<-list(Polyphemus)
for (i in 1:length(Polyphemus)) {
  
  Polyphemus.df[[i]] = raster::as.data.frame(Polyphemus[[i]],na.rm=TRUE)
  
}
CAMS.df<-list(CAMS)
for (i in 1:length(CAMS)) {
  CAMS.df[[i]] = raster::as.data.frame(CAMS[[i]],na.rm=TRUE)
}

Station.df<-list(Station)
for (i in 1:length(Station)) {
  Station.df[[i]] = raster::as.data.frame(Station[[i]],na.rm=TRUE)
}

for (i in 1:length(Polyphemus)) {
  Polyphemus.df[[i]]<-as.matrix(Polyphemus.df[[i]])
  
}
for (i in 1:length(CAMS)) {
  CAMS.df[[i]]<-as.matrix(CAMS.df[[i]])
  
}


for (i in 1:length(Station)) {
  Station.df[[i]]<-as.matrix(Station.df[[i]])
  
}
# calculating Mean Bias
Bias_PS<-list(Polyphemus.df,Station.df)
for (i in 1:length(Polyphemus)) {
  for (j in 1:length(Station)) {
    Bias_PS[[j]]<-Mean_Bias(Polyphemus.df[[j]],Station.df[[j]])
    
  }
  
}
Bias_CS<-list(CAMS.df,Station.df)
for (i in 1:length(CAMS)) {
  for (j in 1:length(Station)) {
    Bias_CS[[j]]<-Mean_Bias(CAMS.df[[j]],Station.df[[j]])
    
  }
  
}

a<-unlist(Bias_PS)
b<-unlist(Bias_CS)
a
b
month <- seq(as.Date("2016-06-01"), 
             as.Date("2018-12-31"), 
             by = "1 month")
month
PS.df<-data.frame(month<-month, a<-a[1:31])
CS.df<-data.frame(month<-month, b<-b[1:31])
# plotting
png(file="E:/Master Thesis/Bias PM2.51 2016 - 2018.png",width = 1224,height = 571)
colors<-c("Polyphemus vs Station"="green","CAMS vs Station"="red")
ggplot() + 
  geom_line(data=PS.df, aes(x=month, y=a,color="Polyphemus vs Station" ) ) + 
  geom_line(data=CS.df, aes(x=month, y=b,color="CAMS vs Station")  )+
  scale_x_date(date_breaks="1 month", date_labels="%b-%y") +
  labs(title="Bias PM2.5 (Polyphemus, CAMS Reanalysis vs Station Data) June 2016 - Dec 2018 ",
       x="Month",y="Concentration (µg/m3)",colour="Model:")+theme(legend.position = "bottom" )+scale_color_manual(values = colors)
dev.off()
########

#map

bias_PS_raster<-list(Polyphemus,Station)

for (i in 1:length(Polyphemus)) { # calculating pixel wise mean bias for model vs station
  for (j in 1:length(Station)) {
    bias_PS_raster[[j]]<-Mean_Bias(Polyphemus[[j]],Station[[j]])
    
  }
  
}
bias_CS_raster<-list(CAMS,Station)

for (i in 1:length(CAMS)) {
  for (j in 1:length(Station)) {
    bias_CS_raster[[j]]<-Mean_Bias(CAMS[[j]],Station[[j]])
    
  }
  
}

Pnames <- list.files("E:/Master Thesis/POLYPHEMUS/Domain2/NO2 Monthly data Regridded/",pattern="*.nc",full.names=FALSE)
Pnames

#Mean Bias Map visualization 
zlim<-list(bias_PS_raster)
for (i in 1:31) {#plots with common color bars.
  
  mypath <- file.path("E:","Master Thesis",paste("Bias_", Pnames[[i]], ".png", sep = ""))
  #par(mar=c(0,0,0,0))
  png(file=mypath,width= 900, # defining jpeg quality 1224x571
      height    = 571)
  
  par(mfrow=c(1,2) )
  
  zlim[[i]] <- range(values(bias_PS_raster[[i]]))
  Polyphemus[[i]]<-raster::plot(bias_PS_raster[[i]], main =  (paste0("Bias_Polyphemus vs Station_",Pnames[[i]])), xlab = "lon", ylab="lat",bty="n",zlim=zlim[[i]],legend=FALSE,col=topo.colors(41),asp=0)
  
  maps::map(add=TRUE, col="black") # country lines specifications and permission to define in  the above image plot
  # Create points of interest
  place <- c("Paris", "Zurich", "Munich", "Brussels", "Milan", "Cologne","Frankfurt","London","Prague","Vienna","Essen") #Essen: 51.450833, 7.013056
  p.lon <- c(2.346954, 8.55,  11.566667, 4.35,9.19, 6.952778,8.683333,-0.1275,14.416667,16.366667,7.013056)
  p.lat <- c(48.857035, 47.366667, 48.133333, 50.85, 45.466944, 50.936389,50.116667,51.507222,50.083333,48.2,51.450833)
  # Add points to map
  points(p.lon, p.lat, pch=20, col="black", bg="blue", cex=1)
  # Add place names to map
  text(p.lon, p.lat, labels=place, cex=0.80, pos=1) 
  
  CAMS[[i]]<-raster::plot(bias_CS_raster[[i]], main = (paste0("Bias_CAMS vs Station_",Pnames[[i]])), xlab = "lon", ylab="lat",bty="n",zlim=zlim[[i]], legend=FALSE,col=topo.colors(41),asp=0)
  
  maps::map(add=TRUE, col="black") # country lines specifications and permission to define in  the above image plot
  # Create points of interest
  place <- c("Paris", "Zurich", "Munich", "Brussels", "Milan", "Cologne","Frankfurt","London","Prague","Vienna","Essen") #Essen: 51.450833, 7.013056
  p.lon <- c(2.346954, 8.55,  11.566667, 4.35,9.19, 6.952778,8.683333,-0.1275,14.416667,16.366667,7.013056)
  p.lat <- c(48.857035, 47.366667, 48.133333, 50.85, 45.466944, 50.936389,50.116667,51.507222,50.083333,48.2,51.450833)
  # Add points to map
  points(p.lon, p.lat, pch=20, col="black", bg="blue", cex=1)
  # Add place names to map
  text(p.lon, p.lat, labels=place, cex=0.80, pos=1) 
  plot(bias_PS_raster[[i]], zlim=zlim[[i]],  legend.only=TRUE,inset = c(0, -0.3),legend.width=2, legend.shrink=0.50,col=topo.colors(41),legend.args = list(text='NO2 (µg/m3)',pch=16,font=1.5, line=2.5, cex=0.8)) 
  
  dev.off()
}

#####

