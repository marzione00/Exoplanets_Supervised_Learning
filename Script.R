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
library(randomForestExplainer)
library(rpart.plot)
library(visreg)
library(rfPermute)
library(rattle)
library(PerformanceAnalytics)
library(e1071)

Planets_dataset <- data.frame(read_excel("C:/Users/Marzio/Desktop/Planets/phl_exoplanet_catalogR.xlsx"))

set.seed(2)

Planets_dataset_train<- sample(499,400)
Planets_dataset_test<-Planets_dataset[-Planets_dataset_train,]

chart.Correlation(Planets_dataset[,2:14], histogram=FALSE)


tree.planet <- rpart(P_H~P_P+S_T+P_D+P_PN+P_A+P_D_E+P_F+P_T_E+S_R_E+S_L+P_R+P_M,data=Planets_dataset,method="class", subset=Planets_dataset_train,minsplit = 5)

rfor.planet <-randomForest(as.factor(P_H)~P_P+S_T+P_D+P_PN+P_A+P_D_E+P_F+P_T_E+S_R_E+S_L+P_R+P_M,data=Planets_dataset, subset=Planets_dataset_train,localImp = TRUE,importance=TRUE,proximity=TRUE)


pca.planet <- prcomp(Planets_dataset[,2:14], center = TRUE,scale. = TRUE)

#svm.planet <- svm( data=Planets_dataset, subset=Planets_dataset_train, kernel="radial")
#summary(svm.planet)
#plot(svm.planet,mode="pca")

fancyRpartPlot(tree.planet,sub = "Planets Habitability", palettes = "OrRd")

#explain_forest(rfor.planet)
plot(rfor.planet)
legend("top", colnames(rfor.planet$err.rate), fill=1:ncol(rfor.planet$err.rate))
varImpPlot(rfor.planet)
proximityPlot(rfor.planet)



print(rfor.planet)
print(importance(rfor.planet,type=2))

#printcp(tree.planet)

rpart.plot(tree.planet,box.palette=c("red", "green"),digits=4,extra=106)


tree.predict<-predict(tree.planet, Planets_dataset_test, type = "class")
rfor.predict<-predict(rfor.planet, Planets_dataset_test, type = "class")
caret::confusionMatrix(table(tree.predict,Planets_dataset_test[,12]))
caret::confusionMatrix(table(rfor.predict,Planets_dataset_test[,12]))






















Planets_dataset <- data.frame(read_excel("C:/Users/Marzio/Desktop/Planets/phl_exoplanet_catalogR.xlsx"))
Planets_dataset_test_SET <- data.frame(read_excel("C:/Users/Marzio/Desktop/Planets/phl_exoplanet_catalog_test.xlsx"))

set.seed(1)


#caret.control <- trainControl(method = "cv",number=4)

#rpart.cv <- train(as.factor(P_HABITABLE)~P_DISTANCE+P_PERIASTRON+P_APASTRON+P_DISTANCE_EFF+P_FLUX+P_TEMP_EQUIL+S_RADIUS_EST+S_LUMINOSITY, data = Planets_dataset,method = "rpart",trControl = caret.control)
rpart.cv <- train(as.factor(P_HABITABLE)~P_DISTANCE+P_PERIASTRON+P_APASTRON+P_DISTANCE_EFF+P_FLUX+P_TEMP_EQUIL+S_RADIUS_EST+S_LUMINOSITY, data = Planets_dataset,method = "rpart")


#rpart.best <- rpart.cv$finalModel

prp(rpart.best, type = 5, extra = 1, under = TRUE)


tree.predict<-predict(rpart.cv,Planets_dataset_test_SET)
peppo<-data.frame(tree.predict)
confusionMatrix(table(tree.predict,Planets_dataset_test_SET[,12]))












palette = colorRampPalette(c("green", "blue", "red")) (20)
heatmap(x = cor(Planets_dataset[,2:15]), col = palette, symm = TRUE, margins = c(10, 10), main = 'Planets',dist(Planets_dataset[,3:15],method = 'euclidean'))

