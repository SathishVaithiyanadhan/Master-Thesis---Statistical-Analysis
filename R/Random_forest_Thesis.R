library(magrittr)
library(dplyr)
library(caret)
library(randomForest)
library(Metrics)
library(ggpubr)
library(ggplot2)

emissions<-lapply(Sys.glob("E:/Master Thesis/Parameters/*.csv"),read.csv)
length(emissions)
#head(emissions[[1]])
for (i in 1:length(emissions)) { # applying na values as 0
  emissions[[i]][][is.na(emissions[[i]])]<-0
  
}


parameters<-emissions[[8]][1:11]
summary(parameters)
#head(parameters)
names(emissions[[5]])
#splitting parameters for features and target
X<-parameters %>% dplyr::select(Precipitation,Boundary.Height,Wind,Solar.Radiation,Surface.Pressure,Surface.Temperature,Month,Season)
y<-parameters$Station_Concentration
z<-parameters$Polyphemus_concentration # to compare the predicted values with original Polyphemus datasets
#Training the model and making prediction
#split training and test data
index<-createDataPartition(y,p=0.75,list = FALSE)
X_train<-X[ index,]
X_test<-X[-index, ]
y_train<-y[index]
y_test<-y[-index]
z_train<-z[index]
z_test<-z[-index]

#model training
RF_regression<-randomForest(x = X_train,y = y_train, maxnodes = 3000, ntree= 3000)# training RF regression model.
RF_regression

#make prediction
predictions<-predict(RF_regression,X_test,interval = "prediction")# predicting concentrations using the trained model
head(predictions)
result<-X_test
head(result)
result['Station']<-y_test # combining the predicted values with the testing datasets.
result['prediction']<-predictions
head(result)
length(result$prediction)
cdf<-as.data.frame(cbind(y_test,predictions)) 
png(file="E:/Master Thesis/PM10 Southern France y_test vs Prediction plot.png",width = 700,height = 500) # plotting correlation between predcited and the station data.
ggscatter(cdf, x = "z_test", y = "predictions", 
          add = "reg.line", add.params = list(color = "red"), conf.int = TRUE, 
          cor.coef = TRUE, cor.coeff.args = list(method = "pearson",  label.sep = "\n"),title = "PM10 Southern France Station Conc. (y_test) Vs Predicted Conc.",
          xlab = "Station Concentration (µg/m3)", ylab = "Predicted Concentration (µg/m3)")
dev.off()
#calculating Accuracy assessment using standard statistical indicators
RMSE = function(x, y){
  sqrt(mean((x - y)^2,na.rm=TRUE))
}

MeanBias = function(x,y){
  mean(x-y,na.rm=TRUE)
}

fge = function(x,y){
  2*mean(abs((x-y)/(x+y)),na.rm=TRUE)
}

print(paste0('Correlation:', cor(y_test,predictions,method = "pearson")))
print(paste0('Root Mean Square Error:', RMSE(y_test,predictions)))
print(paste0('Mean Bias:', MeanBias(y_test,predictions)))
print(paste0('Fractional Gross Error:', fge(y_test,predictions)))
print(paste0('Mean Absolute Error:', mae(y_test,predictions)))


#verifying Feature importance
Gini<-importance(reg) #calculating gini importance
Gini
Feature_importance<-data.frame(Feature=row.names(Gini),Importance=Gini[ ,1]) # arranging feature importance 
Feature_importance
Importance_plot <- ggplot(Feature_importance, # Feature importance plot
                aes(x= reorder(Feature,
                               Importance) , y = Importance) ) +
  geom_bar(stat = "identity", fill = "#dc143c") + # color selection
  coord_flip() +
  theme_light(base_size = 70) +
  xlab("") + 
  ylab("Importance")+
  ggtitle("PM10 Southern France Feature importance in Prediction values\n") +
  theme(plot.title = element_text(size=70))
#Importance_plot

ggsave("PM10 Southern France Poly Feature Importance without date.png", Imp_plot,width = 1224,height = 700,units = "mm")

###########################
png(file="E:/PM10 Southern France Poly Station vs Pedicted.png",width = 1224,height = 571)
par(mfrow=c(2,2) )
plot(result$Station, main="Station PM10 - Southern France",ylim=c(1,100))
plot(z_test, main="Polyphemus PM10 - Southern France",ylim=c(1,100))
plot(result$prediction,main="Predicted PM10 - Southern France",ylim=c(1,100))
dev.off()
######################################################################################################
#visualization
#Plot Predicted vs Station 
a<-ggplot()+
  geom_point(aes(x=X_test$Precipitation, y= y_test, color = 'red',alpha = 0.5))+
  geom_point(aes(x=X_test$Precipitation, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Precipitation Predicted Vs Station Concentration",x="Precipitation", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Station"),values=c("blue","red"))+
  theme(text = element_text(size = 30))

b<-ggplot()+
  geom_point(aes(x=X_test$Boundary.Height, y= y_test, color = 'red',alpha = 0.5))+
  geom_point(aes(x=X_test$Boundary.Height, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Boundary height Predicted Vs Station Concentration",x="Boundary Height", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Station"),values=c("blue","red"))+
  theme(text = element_text(size = 20))

c<-ggplot()+
  geom_point(aes(x=X_test$Wind, y= y_test, color = 'red',alpha = 0.5))+
  geom_point(aes(x=X_test$Wind, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Wind Predicted Vs Station Concentration",x="Wind", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Station"),values=c("blue","red"))+
  theme(text = element_text(size = 20))
d<-ggplot()+
  geom_point(aes(x=X_test$Solar.Radiation, y= y_test, color = 'red',alpha = 0.5))+
  geom_point(aes(x=X_test$Solar.Radiation, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Solar Radiation Predicted Vs Station Concentration",x="Solar Radiation", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Station"),values=c("blue","red"))+
  theme(text = element_text(size = 20))
e<-ggplot()+
  geom_point(aes(x=X_test$Surface.Pressure, y= y_test, color = 'red',alpha = 0.5))+
  geom_point(aes(x=X_test$Surface.Pressure, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Surface Pressure Predicted Vs Station Concentration",x="Surface Pressure", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Station"),values=c("blue","red"))+
  theme(text = element_text(size = 20))
f<-ggplot()+
  geom_point(aes(x=X_test$Surface.Temperature, y= y_test, color = 'red',alpha = 0.5))+
  geom_point(aes(x=X_test$Surface.Temperature, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Surface Temperature Predicted Vs Station Concentration",x="Surface Temperature", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Station"),values=c("blue","red"))+
  theme(text = element_text(size = 20))


Image<-ggarrange(a,b,c,d,e,f, nrow=3,ncol=2,common.legend = TRUE, legend="bottom")
ggsave("PM10 Southern France Station vs Predicted Parameters plot.png", Image,width = 1224,height = 700,units = "mm")
###################################################################################################################################
#plot Predicted vs station and polyphemus Model
a2<-ggplot()+
  geom_point(aes(x=X_test$ï..Date, y= y_test, color = 'red',alpha = 0.5))+
  geom_point(aes(x=X_test$ï..Date, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Predicted Vs Station Concentration",x="Date", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Station Conc."),values=c("blue","red"))+
  theme(text = element_text(size = 30))

a22<-ggplot()+
  geom_point(aes(x=X_test$ï..Date, y= z_test, color = 'green',alpha = 0.5))+
  geom_point(aes(x=X_test$ï..Date, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Predicted Vs Polyphemus Concentration",x="Date", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Polyphemus Conc."),values=c("blue","black"))+
  theme(text = element_text(size = 30))
Image1<-ggarrange(a2,a22, nrow=2,ncol=1, legend="bottom")
ggsave("PM10 Southern France station_Poly_Predicted Date.png", Image1,width = 1224,height = 700,units = "mm")

###########################################################################################################
#PLot predicted vs polyphemus
a1<-ggplot()+
  geom_point(aes(x=X_test$Precipitation, y= z_test, color = 'red',alpha = 0.5))+
  geom_point(aes(x=X_test$Precipitation, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Precipitation Predicted Vs Polyphemus Concentration",x="Precipitation", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Polyphemus"),values=c("blue","red"))+
  theme(text = element_text(size = 30))

b1<-ggplot()+
  geom_point(aes(x=X_test$Boundary.Height, y= z_test, color = 'red',alpha = 0.5))+
  geom_point(aes(x=X_test$Boundary.Height, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Boundary height Predicted Vs Polyphemus Concentration",x="Boundary Height", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Polyphemus"),values=c("blue","red"))+
  theme(text = element_text(size = 20))

c1<-ggplot()+
  geom_point(aes(x=X_test$Wind, y= z_test, color = 'red',alpha = 0.5))+
  geom_point(aes(x=X_test$Wind, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Wind Predicted Vs Polyphemus Concentration",x="Wind", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Polyphemus"),values=c("blue","red"))+
  theme(text = element_text(size = 20))
d1<-ggplot()+
  geom_point(aes(x=X_test$Solar.Radiation, y= z_test, color = 'red',alpha = 0.5))+
  geom_point(aes(x=X_test$Solar.Radiation, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Solar Radiation Predicted Vs Polyphemus Concentration",x="Solar Radiation", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Polyphemus"),values=c("blue","red"))+
  theme(text = element_text(size = 20))
e1<-ggplot()+
  geom_point(aes(x=X_test$Surface.Pressure, y= z_test, color = 'red',alpha = 0.5))+
  geom_point(aes(x=X_test$Surface.Pressure, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Surface Pressure Predicted Vs Polyphemus Concentration",x="Surface Pressure", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Polyphemus"),values=c("blue","red"))+
  theme(text = element_text(size = 20))
f1<-ggplot()+
  geom_point(aes(x=X_test$Surface.Temperature, y= z_test, color = 'red',alpha = 0.5))+
  geom_point(aes(x=X_test$Surface.Temperature, y= predictions, color = 'blue',alpha = 0.5))+
  labs(title="PM10 Southern France Surface Temperature Predicted Vs Polyphemus Concentration",x="Surface Temperature", y="Conc (µg/m3)", color= "", alpha= 'Transperency')+
  scale_color_manual(labels = c("predicted","Polyphemus"),values=c("blue","red"))+
  theme(text = element_text(size = 20))


Image2<-ggarrange(a1,b1,c1,d1,e1,f1, nrow=3,ncol=2,common.legend = TRUE, legend="bottom")
#Image
ggsave("PM10 Southern France Poly vs predicted Parameters plot.png", Image2,width = 1224,height = 700,units = "mm")
