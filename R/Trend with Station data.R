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
#Loading NetCDF files

Polyphemus<-lapply(Sys.glob("E:/Master Thesis/Station Data/POLYPHEMUS/NO2/*.nc"),raster::brick,level=1)

CAMS<-lapply(Sys.glob("E:/Master Thesis/Station Data/CAMS/NO2/*.nc"),raster::brick,level=1)

Station<-lapply(Sys.glob("E:/Master Thesis/Station Data/NO2/*.nc"),raster::brick,level=1)

# Defining Function to perform Pixel wise Trend test.
Mann_Kendall <- function(y){unlist(MannKendall(y))}

POLY1<-brick(Polyphemus)
CAMS1<-brick(CAMS)
Station1<-brick(Station)

#Calcualting pixel wise Trend
Poly_trend_Yearly<- calc(POLY1,Mann_Kendall)
names(Poly_trend_Yearly)<-c("Kendall Slope Score","P-value","Kendall's Tau Statistics","Denominator","Variance of Kendall Slope") #defining the names for trend results
CAMS_trend_Yearly<- calc(CAMS1,Mann_Kendall)
names(CAMS_trend_Yearly)<-c("Kendall Slope Score","P-value","Kendall's Tau Statistics","Denominator","Variance of Kendall Slope")
Station_trend_Yearly<- calc(Station1,Mann_Kendall)
names(Station_trend_Yearly)<-c("Kendall Slope Score","P-value","Kendall's Tau Statistics","Denominator","Variance of Kendall Slope")

Poly_trend_Yearly$P.value[Poly_trend_Yearly$P.value >= 0.05] <- NA # defining P-value.
Poly_trend_Yearly$Kendall.Slope.Score[Poly_trend_Yearly$Kendall.Slope.Score >= 0.8] <- NA
CAMS_trend_Yearly$P.value[CAMS_trend_Yearly$P.value >= 0.05] <- NA
CAMS_trend_Yearly$Kendall.Slope.Score[CAMS_trend_Yearly$Kendall.Slope.Score >= 0.8] <- NA
Station_trend_Yearly$P.value[Station_trend_Yearly$P.value >= 0.05] <- NA
Station_trend_Yearly$Kendall.Slope.Score[Station_trend_Yearly$Kendall.Slope.Score >= 0.8] <- NA

#plotting trend maps with p-value

png(file="E:/NO2 Trend POLY CAMS STATION June 2016 - Dec 2018.png",width = 700,height = 571)

par(mfrow=c(2,3))
plot(Poly_trend_Yearly$Kendall.Slope.Score,asp=0,col=topo.colors(40),main = "Poly NO2 Trend June 2016 - Dec 2018", xlab = "lon", ylab="lat",bty="n",legend=TRUE,legend.args = list(text='NO2_Conc(µg/m3)',pch=16,font=1.5, line=2.5, cex=0.8),inset = c(0, -0.3),legend.width=1, legend.shrink=0.50)
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
plot(CAMS_trend_Yearly$Kendall.Slope.Score,asp=0,col=topo.colors(40),main = "CAMS NO2 Trend June 2016 - Dec 2018", xlab = "lon", ylab="lat",bty="n",legend=TRUE,legend.args = list(text='NO2_Conc(µg/m3)',pch=16,font=1.5, line=2.5, cex=0.8),legend.width=1, legend.shrink=0.50)
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
plot(Station_trend_Yearly$Kendall.Slope.Score,asp=0,col=topo.colors(40),main = "Station NO2 Trend June 2016 - Dec 2018", xlab = "lon", ylab="lat",bty="n",legend=TRUE,legend.args = list(text='NO2_Conc(µg/m3)',pch=16,font=1.5, line=2.5, cex=0.8),legend.width=1, legend.shrink=0.50)
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
plot(Poly_trend_Yearly$P.value,asp=0,col=topo.colors(40),main = "Poly NO2 P-Value June 2016 - Dec 2018", xlab = "lon", ylab="lat",bty="n",legend=TRUE,legend.args = list(text='NO2_Conc(µg/m3)',pch=16,font=1.5, line=2.5, cex=0.8),inset = c(0, -0.3),legend.width=1, legend.shrink=0.50)
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

plot(CAMS_trend_Yearly$P.value,asp=0,col=topo.colors(40),main = "CAMS NO2 P-Value June 2016 - Dec 2018", xlab = "lon", ylab="lat",bty="n",legend=TRUE,legend.args = list(text='NO2_Conc(µg/m3)',pch=16,font=1.5, line=2.5, cex=0.8),legend.width=1, legend.shrink=0.50)
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
plot(Station_trend_Yearly$P.value,asp=0,col=topo.colors(40),main = "Station NO2 P-Value June 2016 - Dec 2018", xlab = "lon", ylab="lat",bty="n",legend=TRUE,legend.args = list(text='NO2_Conc(µg/m3)',pch=16,font=1.5, line=2.5, cex=0.8),legend.width=1, legend.shrink=0.50)
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

dev.off()

#plot
#calculating Yearly trend Statistics
df<-raster::as.data.frame(CAMS1,na.rm=TRUE)
df1<-raster::as.data.frame(POLY1, na.rm=TRUE)
df2<-raster::as.data.frame(Station1, na.rm=TRUE)
#df1 <-df1 %>% select(-(x:y)) 
NO2_CAMS_2016_2018<-colMeans(df)
NO2_POLY_2016_2018<-colMeans(df1)
NO2_Station_2016_2018<-colMeans(df2)
Trend_NO2_C<-mk.test(NO2_CAMS_2016_2018)
Trend_NO2_P<-mk.test(NO2_POLY_2016_2018)
Trend_NO2_S<-mk.test(NO2_Station_2016_2018)
t2C<-MannKendall(NO2_CAMS_2016_2018)
Trend_NO2_C
Trend_NO2_P
Trend_NO2_S


#trend plots


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

Polyphemus_Mean<-list(Polyphemus.df)
for (i in 1:length(Polyphemus)) {
  Polyphemus_Mean[[i]]<-mean(as.matrix(Polyphemus.df[[i]]),na.rm=TRUE)
  
}
CAMS_Mean<-list(CAMS.df)

for (i in 1:length(CAMS)) {
  CAMS_Mean[[i]]<-mean(as.matrix(CAMS.df[[i]]),na.rm=TRUE)
  
}
Station_Mean<-list(Station.df)

for (i in 1:length(Station)) {
  Station_Mean[[i]]<-mean(as.matrix(Station.df[[i]]),na.rm=TRUE)
  
}


Trend_P<-unlist(Polyphemus_Mean)
Trend_C<-unlist(CAMS_Mean)
Trend_S<-unlist(Station_Mean)

month <- seq(as.Date("2016-06-01"), 
             as.Date("2018-12-31"), 
             by = "1 month")
month
Trend_Poly<-data.frame(month <- month,Trend_P<-Trend_P[c(1:31)]) 
Trend_CAMS<-data.frame(month <- month,Trend_C<-Trend_C[c(1:31)]) 
Trend_Station<-data.frame(month <- month,Trend_S<-Trend_S[c(1:31)]) 


# CAMS, Polyphemus and station data Trend plots from June 2016 to Dec 2018.
#trend plot using Method: loess and Formula: y ~ x
png(file="E:/PM2.5  Trend  POLY CAMS Station PLOT Curve June 2016 - Dec 2018.png",width = 1224,height = 571)

x<-ggplot(Trend_Poly, aes(x=month, y=Trend_P)) +geom_line()+geom_point() +
  geom_smooth()+scale_x_date(date_breaks="1 month", date_labels="%b-%y") +
  labs(title="Trend PM2.5  Polyphemus June 2016 to Dec 2018 (Method: loess and Formula: y ~ x) ",
       x="Month",y="Concentration (µg/m3)")+theme(axis.text.x=element_blank())+theme(axis.title.x=element_blank())+theme(axis.title.y= element_text(size = 15))


y<-ggplot(Trend_CAMS, aes(x=month, y=Trend_C)) +geom_line()+geom_point() +
  geom_smooth()+scale_x_date(date_breaks="1 month", date_labels="%b-%y") +
  labs(title="Trend PM2.5 CAMS June 2016 to Dec 2018 (Method: loess and Formula: y ~ x)",
       x="Month",y="Concentration (µg/m3)")+theme(axis.text.x=element_blank())+theme(axis.title.x=element_blank())+theme(axis.title.y= element_text(size = 15))

z<-ggplot(Trend_Station, aes(x=month, y=Trend_S)) +geom_line()+geom_point() +
  geom_smooth()+scale_x_date(date_breaks="1 month", date_labels="%b-%y") +
  labs(title="Trend PM2.5 Station June 2016 to Dec 2018 (Method: loess and Formula: y ~ x)",
       x="Month",y="Concentration (µg/m3)")+theme(axis.text.x= element_text(angle=90,hjust=1,vjust=0.5,size=15))+theme(axis.title.y= element_text(size = 15))+theme(axis.title.x= element_text(size = 15))

grid.arrange(x, y, z, nrow=3)
dev.off()

##
#Trend plot using Method: Linear Model and Formula: y ~ x
png(file="E:/NO2 Trend  POLY CAMS Station PLOT LM June 2016 - Dec 2018.png",width = 1224,height = 571)

x1<-ggplot(Trend_Poly, aes(x=month, y=Trend_P)) +geom_line()+geom_point() +
  geom_smooth(method = lm)+scale_x_date(date_breaks="1 month", date_labels="%b-%y") +
  labs(title="Trend NO2 POLY Jun 2016 to Dec 2018 (Method: Linear Model and Formula: y ~ x) ",
       x="Month",y="Concentration (µg/m3)")


y1<-ggplot(Trend_CAMS, aes(x=month, y=Trend_C)) +geom_line()+geom_point() +
  geom_smooth(method = lm)+scale_x_date(date_breaks="1 month", date_labels="%b-%y") +
  labs(title="Trend NO2 CAMS Jun 2016 to Dec 2018 (Method: Linear Model and Formula: y ~ x)",
       x="Month",y="Concentration (µg/m3)")

z1<-ggplot(Trend_Station, aes(x=month, y=Trend_S)) +geom_line()+geom_point() +
  geom_smooth(method = lm)+scale_x_date(date_breaks="1 month", date_labels="%b-%y") +
  labs(title="Trend NO2 Station Jun 2016 to Dec 2018 (Method: Linear Model and Formula: y ~ x)",
       x="Month",y="Concentration (µg/m3)")

grid.arrange(z1, y1, x1, nrow=3)
dev.off()
