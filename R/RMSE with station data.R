library(raster)
library(ncdf4)
library(tidyverse)
library(cowplot)
library(rlang)
library(fields)
library(maps)
library(Metrics)

#Loading NetCDF files
PNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/POLYPHEMUS/NO2/*.nc"),raster::brick,level=1)

CNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/CAMS/NO2/*.nc"),raster::brick,level=1)

SNO2<-lapply(Sys.glob("E:/Master Thesis/Station Data/NO2/*.nc"),raster::brick,level=1)


RMSE = function(x, y){ # defining RMSE formula in a function.
  sqrt(mean((x - y)^2,na.rm=TRUE))
}


#plot
# Conerting NetCDF to DF
PNO2.df<-list(PNO2)
for (i in 1:length(PNO2)) {
  
  PNO2.df[[i]] = raster::as.data.frame(PNO2[[i]],na.rm=TRUE)
  
}
CNO2.df<-list(CNO2)
for (i in 1:length(CNO2)) {
  CNO2.df[[i]] = raster::as.data.frame(CNO2[[i]],na.rm=TRUE)
}

SNO2.df<-list(SNO2)
for (i in 1:length(SNO2)) {
  SNO2.df[[i]] = raster::as.data.frame(SNO2[[i]],na.rm=TRUE)
}

for (i in 1:length(PNO2)) {
  PNO2.df[[i]]<-as.matrix(PNO2.df[[i]])
  
}
for (i in 1:length(CNO2)) {
  CNO2.df[[i]]<-as.matrix(CNO2.df[[i]])
  
}


for (i in 1:length(SNO2)) {
  SNO2.df[[i]]<-as.matrix(SNO2.df[[i]])
  
}
 # Calculate RMSE using the function defined above.
RMSE_PS_NO2<-list(PNO2.df,SNO2.df)
RMSE_PS_NO2
for (i in 1:length(PNO2)) {
  for (j in 1:length(SNO2)) {
    RMSE_PS_NO2[[j]]<-RMSE(PNO2.df[[j]],SNO2.df[[j]])
      
  }
  
}
RMSE_CS_NO2<-list(CNO2.df,SNO2.df)
RMSE_CS_NO2
for (i in 1:length(CNO2)) {
  for (j in 1:length(SNO2)) {
    RMSE_CS_NO2[[j]]<-RMSE(CNO2.df[[j]],SNO2.df[[j]])
    
  }
  
}

a<-unlist(RMSE_PS_NO2)
b<-unlist(RMSE_CS_NO2)
month <- seq(as.Date("2016-06-01"), 
             as.Date("2018-12-31"), 
             by = "1 month")
month
PS.df<-data.frame(month<-month, a<-a[1:31])
CS.df<-data.frame(month<-month, b<-b[1:31])
png(file="E:/Master Thesis/RMSE NO2 2016 - 2018.png",width = 1224,height = 571)
colors<-c("Polyphemus vs Station"="green","CAMS vs Station"="red")
ggplot() + 
  geom_line(data=PS.df, aes(x=month, y=a,color="Polyphemus vs Station" ) ) + 
  geom_line(data=CS.df, aes(x=month, y=b,color="CAMS vs Station")  )+
  scale_x_date(date_breaks="1 month", date_labels="%b-%y") +
  labs(title="RMSE NO2 (Polyphemus, CAMS Reanalysis vs Station Data) June 2016 - Dec 2018 ",
       x="Month",y="RMSE",colour="Model:")+theme(axis.text=element_text(angle=90,hjust=1,vjust=0.5,size=20))+theme(legend.position = "bottom" )+scale_color_manual(values = colors)+theme(axis.title.x= element_text(size = 20,margin = margin(t =50)))+
  theme(legend.text = element_text(size = 20))+theme(legend.title = element_text(size = 20))+theme(plot.title = element_text(size = 20))+theme(axis.title.y= element_text(size = 20))
dev.off()
######


# Map
rmse_NO2_PS<-list(PNO2,SNO2)

for (i in 1:length(PNO2)) { #calculating RMSE using the function defined above.
  for (j in 1:length(SNO2)) {
    rmse_NO2_PS[[j]]<-RMSE(PNO2[[j]],SNO2[[j]])
    
  }
  
}
rmse_NO2_CS<-list(CNO2,SNO2)

for (i in 1:length(CNO2)) {
  for (j in 1:length(SNO2)) {
    rmse_NO2_CS[[j]]<-RMSE(CNO2[[j]],SNO2[[j]])
    
  }
  
}

PNO2names <- list.files("E:/Master Thesis/POLYPHEMUS/Domain2/PM10 Monthly data Regridded/",pattern="*.nc",full.names=FALSE)
PNO2names

# RMSE Map visualization
zlim<-list(rmse_NO2_PS)
for (i in 1:31) {#plots with common color bars.
  
  mypath <- file.path("E:","Master Thesis",paste("RMSE_", PNO2names[[i]], ".png", sep = ""))
  #par(mar=c(0,0,0,0))
  png(file=mypath,width= 900, # defining jpeg quality 1224x571
      height    = 571)
  
  par(mfrow=c(1,2) )
  
  pal <- colorRampPalette(c("red","orange","pink","magenta","honeydew","aquamarine3","purple","blue","cyan","mediumorchid2","green","yellow"))
  
  zlim[[i]] <- range(values(rmse_NO2_PS[[i]]))
  PNO2[[i]]<-raster::plot(rmse_NO2_PS[[i]], main =  (paste0("RMSE_Polyphemus vs Station_",PNO2names[[i]])), xlab = "lon", ylab="lat",bty="n",zlim=zlim[[i]],legend=FALSE,col=topo.colors(41),asp=0)
  
  maps::map(add=TRUE, col="black") # country lines specifications and permission to define in  the above image plot
  # Create points of interest
  place <- c("Paris", "Zurich", "Munich", "Brussels", "Milan", "Cologne","Frankfurt","London","Prague","Vienna","Essen") #Essen: 51.450833, 7.013056
  p.lon <- c(2.346954, 8.55,  11.566667, 4.35,9.19, 6.952778,8.683333,-0.1275,14.416667,16.366667,7.013056)
  p.lat <- c(48.857035, 47.366667, 48.133333, 50.85, 45.466944, 50.936389,50.116667,51.507222,50.083333,48.2,51.450833)
  # Add points to map
  points(p.lon, p.lat, pch=20, col="black", bg="blue", cex=1)
  # Add place names to map
  text(p.lon, p.lat, labels=place, cex=0.80, pos=1) 
  
  CNO2[[i]]<-raster::plot(rmse_NO2_CS[[i]], main = (paste0("RMSE_CAMS vs Station_",PNO2names[[i]])), xlab = "lon", ylab="lat",bty="n",zlim=zlim[[i]], legend=FALSE,col=topo.colors(41),asp=0)
  
  maps::map(add=TRUE, col="black") # country lines specifications and permission to define in  the above image plot
  # Create points of interest
  place <- c("Paris", "Zurich", "Munich", "Brussels", "Milan", "Cologne","Frankfurt","London","Prague","Vienna","Essen") #Essen: 51.450833, 7.013056
  p.lon <- c(2.346954, 8.55,  11.566667, 4.35,9.19, 6.952778,8.683333,-0.1275,14.416667,16.366667,7.013056)
  p.lat <- c(48.857035, 47.366667, 48.133333, 50.85, 45.466944, 50.936389,50.116667,51.507222,50.083333,48.2,51.450833)
  # Add points to map
  points(p.lon, p.lat, pch=20, col="black", bg="blue", cex=1)
  # Add place names to map
  text(p.lon, p.lat, labels=place, cex=0.80, pos=1) 
  plot(rmse_NO2_PS[[i]], zlim=zlim[[i]],  legend.only=TRUE,inset = c(0, -0.3),legend.width=2, legend.shrink=0.50,col=topo.colors(41),legend.args = list(text='PM10 (µg/m3)',pch=16,font=1.5, line=2.5, cex=0.8)) 
  
  dev.off()
}
