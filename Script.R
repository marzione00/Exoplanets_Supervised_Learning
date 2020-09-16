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
library(InformationValue)
library(ROCR)


#########Loading data


Planets_dataset <- data.frame(read_excel("C:/Users/Marzio/Desktop/Planets/phl_exoplanet_catalogR.xlsx"))

set.seed(2)

#########Splitting training vs test set

Planets_dataset_train<- sample(499,300)
Planets_dataset_test<-Planets_dataset[-Planets_dataset_train,]


#########Plotting the correlation chart


chart.Correlation(Planets_dataset[,2:14], histogram=FALSE)



#########Decision Tree 


tree.planet <- rpart(P_H~P_P+S_T+P_D+P_PN+P_A+P_D_E+P_F+P_T_E+S_R_E+S_L+P_R+P_M,data=Planets_dataset,method="class", subset=Planets_dataset_train,minsplit = 5)

fancyRpartPlot(tree.planet,sub = "Planets Habitability", palettes = "OrRd")

tree.predict<-data.frame(predict(tree.planet, Planets_dataset_test, type = "class"))

rpart.plot(tree.planet,box.palette=c("red", "green"),digits=4,extra=106)

tree.predict["Test"]<-as.factor(Planets_dataset_test[,12])

colnames(tree.predict)<-c("Predict","Test")


caret::confusionMatrix(table(tree.predict))

fourfoldplot(table(tree.predict), color = c("red","darkgreen"),conf.level = 0, margin = 1, main = "Decision Tree")

pred_dec<-prediction(as.numeric(tree.predict$Predict),as.numeric(tree.predict$Test))

roc_dec.perf <- performance(pred_dec, measure = "tpr", x.measure = "fpr")

autoplot(roc_dec.perf)+theme_bw()

#########Random Forest


rfor.planet <-randomForest(as.factor(P_H)~P_P+S_T+P_D+P_PN+P_A+P_D_E+P_F+P_T_E+S_R_E+S_L+P_R+P_M,data=Planets_dataset, subset=Planets_dataset_train,localImp = TRUE,importance=TRUE,proximity=TRUE)
rfor.predict<-data.frame(predict(rfor.planet, Planets_dataset_test, type = "class"))
#explain_forest(rfor.planet)
plot(rfor.planet)
legend("top", colnames(rfor.planet$err.rate), fill=1:ncol(rfor.planet$err.rate))
varImpPlot(rfor.planet)
proximityPlot(rfor.planet)
#print(rfor.planet)
#print(importance(rfor.planet,type=2))

rfor.predict["Test"]<-as.factor(Planets_dataset_test[,12])

colnames(rfor.predict)<-c("Predict","Test")


caret::confusionMatrix(table(rfor.predict))

fourfoldplot(table(rfor.predict), color = c("red","darkgreen"),conf.level = 0, margin = 1, main = "Random Forest")

pred_for<-prediction(as.numeric(rfor.predict$Predict),as.numeric(rfor.predict$Test))

roc_for.perf <- performance(pred_for, measure = "tpr", x.measure = "fpr")

autoplot(roc_for.perf)+theme_bw()


#########PCA+SVM 

pca.train<-Planets_dataset[Planets_dataset_train,]
pca.test<-Planets_dataset[-Planets_dataset_train,]
pca.planet <- prcomp(pca.train[,2:14], center = TRUE,scale. = TRUE)
pca.planet.test  <-  predict(pca.planet, pca.test[,2:14])

autoplot(pca.planet,data=pca.train[,2:14],col="P_H")
autoplot(pca.planet.test)

pca_out<-data.frame(pca.planet[["x"]])
pca_out_test<-data.frame(pca.planet.test[["x"]])

train<-pca_out[1:2]
test<-pca_out_test[1:2]
train["H"]<-pca.train[,12]
test["H"]<-pca.test[,12]


svm.planet <- svm(H~., data=train,type = 'C-classification', kernel="linear")
summary(svm.planet)
plot(svm.planet,train)

svm.predict<-data.frame(predict(svm.planet,pca.planet.test[,1:2],type = "class"))
colnames(svm.predict)[1]<-"H"

svm.predict["T"]<-as.factor(pca.test[,12])

svm_fin<-data.frame(svm.predict,stringsAsFactors = TRUE)


colnames(svm_fin)<-c("Predict","Test")


caret::confusionMatrix(table(svm_fin))


fourfoldplot(table(svm_fin), color = c("red","darkgreen"),conf.level = 0, margin = 1, main = "SVM")

pred_svm<-prediction(as.numeric(svm_fin$Predict),as.numeric(svm_fin$Test))

roc_svm.perf <- performance(pred_svm, measure = "tpr", x.measure = "fpr")

autoplot(roc_svm.perf)+theme_bw()


#########Logistic Regression 

glm.planet<- glm(P_H~P_P+S_T+P_D+P_PN+P_A+P_D_E+P_F+P_T_E+S_R_E+S_L+P_R+P_M, data=pca.train[,2:14],family=binomial)
summary(glm.planet)


glm.prob<-data.frame(predict(glm.planet,pca.test,type = "response"))
glm.prob<-ifelse(glm.prob > 0.5, "1", "0")

glm_fin<-data.frame(glm.prob,stringsAsFactors = TRUE)
glm_fin["Test"]<-as.factor(pca.test[,12])

colnames(glm_fin)<-c("Predict","Test")

caret::confusionMatrix(table(glm_fin))

fourfoldplot(table(glm_fin), color = c("red","darkgreen"),conf.level = 0, margin = 1, main = "Logistic")

pred_log<-prediction(as.numeric(glm_fin$Predict),as.numeric(glm_fin$Test))

roc_log.perf <- performance(pred_log, measure = "tpr", x.measure = "fpr")

autoplot(roc_log.perf)+theme_bw()

