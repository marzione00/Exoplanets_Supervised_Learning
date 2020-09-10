library(readxl)
library(ISLR)
library(readxl)
library(devtools)
library(ggfortify)
library(ggrepel)
library(ggplot2)
library(corrplot)
library(factoextra)
library(dendextend)
library(mdendro)
library(rpart)
library(rpart.plot)
library(caret)
library(tree)
library(randomForest)

Planets_dataset <- data.frame(read_excel("C:/Users/Marzio/Desktop/Planets/phl_exoplanet_catalogR.xlsx"))

set.seed(1)

Planets_dataset_train<- sample(3712,2500)
Planets_dataset_test<-Planets_dataset[-Planets_dataset_train,]

tree.planet = rpart(P_HABITABLE~P_DISTANCE+P_PERIASTRON+P_APASTRON+P_DISTANCE_EFF+P_FLUX+P_TEMP_EQUIL+S_RADIUS_EST+S_LUMINOSITY,data=Planets_dataset,method="class", subset=Planets_dataset_train)
rfor<-randomForest(P_HABITABLE~P_DISTANCE+P_PERIASTRON+P_APASTRON+P_DISTANCE_EFF+P_FLUX+P_TEMP_EQUIL+S_RADIUS_EST+S_LUMINOSITY,data=Planets_dataset,method="class", subset=Planets_dataset_train)

print(rfor)
print(importance(rfor,type=2))

printcp(tree.planet)

rpart.plot(tree.planet,box.palette=c("red", "green"),digits=4)


tree.predict<-predict(tree.planet, Planets_dataset_test, type = "class")
confusionMatrix(table(tree.predict,Planets_dataset_test[,14]))

palette = colorRampPalette(c("green", "blue", "red")) (20)
heatmap(x = cor(Planets_dataset[,2:15]), col = palette, symm = TRUE, margins = c(10, 10), main = 'Planets',dist(Planets_dataset[,3:15],method = 'euclidean'))

