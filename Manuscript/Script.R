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
library(logistf)
library(MASS)
library(pca3d)
library(doParallel)
library(kernlab)
library(klaR)
library(PCAmixdata)
library(ggplotify)
library(FactoMineR)
library(plotly)
library(klaR)
library(car)
library(ggpubr)
library(cairoDevice)



phl_exoplanet_catalog_RENAMED <- data.frame(read_excel("phl_exoplanet_catalog_RENAMED.xlsx"),stringsAsFactors = FALSE)
Planet_not_habitable_l<-subset(phl_exoplanet_catalog_RENAMED,P_H==0)
Planet_habitable<-subset(phl_exoplanet_catalog_RENAMED,P_H==1)


Planet_dataset_no_habit_sample<- sample(3650,445)

phl_exoplanet_not_habitable<-Planet_not_habitable_l[Planet_dataset_no_habit_sample,]

Planets_dataset<-rbind(Planet_habitable,phl_exoplanet_not_habitable)

#save(Planets_dataset,file="Planets_dataset.rda")

#Loading a previous dataset were the non-habitable planets
#were randomized among the whole dataset 


#load("Planets_dataset.rda")

Planets_dataset[,12]<-as.factor(Planets_dataset[,12])
Planets_dataset[,15]<-as.factor(Planets_dataset[,15])



set.seed(0)

#########Splitting training vs test set

Planets_dataset_train<- sample(500,350)
Planets_dataset_test<-Planets_dataset[-Planets_dataset_train,]

levels(Planets_dataset$P_H) <- c("False","True")


chart.Correlation(Planets_dataset[,-c(1,12,15)],histogram=TRUE, pch="+")

##############
#Decision Tree
##############



tree.planet <- rpart(P_H~P_P+S_T+P_D+P_PN+P_A+P_D_E+P_F+P_T_E+S_R_E+S_L+P_R+P_M+S_S_T,method="class",data=Planets_dataset, subset=Planets_dataset_train,minsplit = 5)

var_imp_dec_tree<-data.frame(caret::varImp(tree.planet) %>%
                               rownames_to_column() %>%
                               arrange(desc(Overall)) %>%
                               slice(1:10))


ggplot(var_imp_dec_tree, aes(y=reorder(rowname,Overall),x=Overall,color="red")) +
  geom_point() +
  geom_segment(aes(x=0,xend=Overall,yend=rowname)) +
  scale_color_discrete(name="Variable Group") +
  ylab("Overall importance") +
  xlab("Variable Name") + guides(color = FALSE, size = FALSE) + theme_bw()



fancyRpartPlot(tree.planet,sub = "Planets Habitability", palettes = "OrRd")

tree.predict<-data.frame(predict(tree.planet, Planets_dataset_test, type = "class"))

rpart.plot(tree.planet,box.palette=c("red", "green"),digits=4,extra=106)

tree.predict["Test"]<-as.factor(Planets_dataset_test[,12])

colnames(tree.predict)<-c("Predict","Test")


fourfoldplot(table(tree.predict), color = c("red","darkgreen"),conf.level = 0, margin = 1, main = "Decision Tree")


##############
#Random Forest
##############

RF_perf_out<-tuneRF(Planets_dataset[Planets_dataset_train,-c(12,1)],Planets_dataset[Planets_dataset_train,12], ntree=5000)
RF_perf_out<-data.frame(RF_perf_out)
ggplot(RF_perf_out,aes(x=mtry, y=OOBError))+geom_line(color="red",linetype="dashed")+geom_point(color="red")+theme_bw()
rfor.planet <-randomForest(P_H~P_P+S_T+P_D+P_PN+P_A+P_D_E+P_F+P_T_E+S_R_E+S_L+P_R+P_M+S_S_T,data=Planets_dataset, subset=Planets_dataset_train,localImp = TRUE,importance=TRUE,proximity=TRUE,ntry=4)
rfor.predict<-data.frame(predict(rfor.planet, Planets_dataset_test, type = "class"))
#explain_forest(rfor.planet)


var_imp_rforest<-data.frame(varImp(rfor.planet))
colnames(var_imp_rforest)<-c("Variable","Overall")
var_imp_rforest[,1]<-rownames(var_imp_rforest)
rownames(var_imp_rforest)<-seq(1:13)

ggplot(var_imp_rforest, aes(y=reorder(Variable,Overall),x=Overall,color="red")) +
  geom_point() +
  geom_segment(aes(x=0,xend=Overall,yend=Variable)) +
  scale_color_discrete(name="Variable Group") +
  xlab("Overall importance") +
  ylab("Variable Name") + guides(color = FALSE, size = FALSE) + theme_bw()


plot(rfor.planet)
tree_plot<-data.frame(rfor.planet[["err.rate"]])
tree_plot[4]<-seq(1:500)
colnames(tree_plot)<-c("OOB","Not_habitable","Habitable","Trees")




ggplot() + geom_line(data = tree_plot, aes(x = Trees, y = OOB,color = "OOB") ) +
  geom_line(data = tree_plot, aes(x = Trees, y = Not_habitable,color = "Not H") ) +
  geom_line(data = tree_plot, aes(x = Trees, y = Habitable,color = "H") )+labs(color = "Legend")+theme() + xlab('Trees') + ylab('Error')+theme_bw()


plot(rfor.planet)
legend("top", colnames(rfor.planet$err.rate), fill=1:ncol(rfor.planet$err.rate))
varImpPlot(rfor.planet)
proximityPlot(rfor.planet)
print(rfor.planet)

rfor.predict["Test"]<-as.factor(Planets_dataset_test[,12])

colnames(rfor.predict)<-c("Predict","Test")


fourfoldplot(table(rfor.predict), color = c("red","darkgreen"),conf.level = 0, margin = 1, main = "Random Forest")



####
#SVM
####


tune_svm_full.out<-tune(svm ,P_H~P_P+S_T+P_D+P_PN+P_A+P_D_E+P_F+P_T_E+S_R_E+S_L+P_R+P_M+S_S_T,data=Planets_dataset[Planets_dataset_train,], type = 'C-classification',kernel="polynomial",
 ranges =list(cost=(1:10),degree=(1:5)))
print(tune_svm_full.out)
perf_svm<-data.frame(tune_svm_full.out[["performances"]])

ggplot(perf_svm,aes(x=cost,y=degree, z=error))+geom_line(color="red",linetype="dashed")+geom_point(color="red")+theme_bw()

#X11(width=60, height=60)
#plot_ly(perf_svm[,1:3],x = ~cost, y = ~degree, z = ~error, type="scatter3d", mode="markers")


svm.full <- svm(P_H~P_P+S_T+P_D+P_PN+P_A+P_D_E+P_F+P_T_E+S_R_E+S_L+P_R+P_M+S_S_T, data=Planets_dataset[Planets_dataset_train,],type = 'C-classification', kernel="polynomial",cost=4,degree=2,)

svm.predict_full<-data.frame(predict(svm.full,Planets_dataset[-Planets_dataset_train,],type = "class"))

svm.predict_full["T"]<-as.factor(Planets_dataset[-Planets_dataset_train,12])

svm_fin_full<-data.frame(svm.predict_full,stringsAsFactors = TRUE)

colnames(svm_fin_full)<-c("Predict","Test")

caret::confusionMatrix(table(svm_fin_full))

fourfoldplot(table(svm_fin_full), color = c("red","darkgreen"),conf.level = 0, margin = 1, main = "SVM_FULL")




########
#PCA+SVM
########



pca_mix_out<-PCAmix(Planets_dataset[Planets_dataset_train,-c(1,12,15)],Planets_dataset[Planets_dataset_train,c(12,15)],rename.level=TRUE)






pca_mix.planet.test  <-  predict(pca_mix_out, Planets_dataset[-Planets_dataset_train,-c(1,12,15)],Planets_dataset[-Planets_dataset_train,c(12,15)])



plot(pca_mix_out,choice="cor",coloring.var = TRUE,main="All variables")

FAMD_planets.out<-FAMD(Planets_dataset[,-c(1)])

plot(FAMD_planets.out)
fviz_famd_var(FAMD_planets.out, "var", col.var = "contrib",repel=TRUE)

quali.var <- get_famd_var(FAMD_planets.out, "quali.var")

fviz_famd_var(FAMD_planets.out, "quali.var",col.var = "contrib",repel=TRUE)


fviz_famd_var(FAMD_planets.out,"quanti.var", col.var = "cos2",gradient.cols = c("red","orange","blue"),repel = TRUE,col.circle = "black") +theme_bw()


train_mix<-data.frame(pca_mix_out[["ind"]][["coord"]])
train_mix<-train_mix[1:2]


train_mix["H"]<-Planets_dataset[Planets_dataset_train,12]


tune_svm_mix.out=tune(svm ,H~.,data=train_mix, type = 'C-classification',kernel="polynomial",ranges =list(cost=(1:20),degree=(1:5)))

perf_svm<-data.frame(tune_svm_mix.out[["performances"]])

plot_ly(perf_svm[,1:3],x = ~cost, y = ~degree, z = ~error, type="scatter3d", mode="markers")

print(tune_svm_mix.out)
plot(tune_svm_mix.out,type="contour")


svm.planet_mix <- ksvm(H~.,data=train_mix,type = 'C-svc', kernel="polydot",kpar = list(degree = 2),C=1)


plot(svm.planet_mix,data=train_mix)



svm.predict_mix<-data.frame(predict(svm.planet_mix,pca_mix.planet.test[,1:2]))



colnames(svm.predict_mix)[1]<-"H"



svm.predict_mix["T"]<-as.factor(Planets_dataset[-Planets_dataset_train,12])


svm_fin_mix<-data.frame(svm.predict_mix,stringsAsFactors = TRUE)


colnames(svm_fin_mix)<-c("Predict","Test")


caret::confusionMatrix(table(svm_fin_mix))


fourfoldplot(table(svm_fin_mix), color = c("red","darkgreen"),conf.level = 0, margin = 1, main = "SVM")



####
#QDA
####


qda.planet<- qda(P_H~P_P+S_T+P_D+P_PN+P_A+P_D_E+P_F+P_T_E+S_R_E+S_L+P_R+P_M, data=Planets_dataset, subset=Planets_dataset_train)

#print(qda.planet)
#X11(width=60, height=60)
#partimat(P_H~S_T+P_D+P_PN+P_T_E+S_R_E+S_L+P_R+P_M,data=Planets_dataset[Planets_dataset_train,],method="qda",nplots.vert=4)

#partimat(P_H ~ S_L+P_T_E, data=Planets_dataset[Planets_dataset_train,], method="qda")

#plot(qda.planet,P_H~S_L)



qda.prob<-data.frame(predict(qda.planet,Planets_dataset[-Planets_dataset_train,],type = "response"))
qda.prob<-qda.prob["class"]

qda_fin<-data.frame(qda.prob,stringsAsFactors = TRUE)
qda_fin["Test"]<-as.factor(Planets_dataset[-Planets_dataset_train,12])

colnames(qda_fin)<-c("Predict","Test")

caret::confusionMatrix(table(qda_fin))

fourfoldplot(table(qda_fin), color = c("red","darkgreen"),conf.level = 0, margin = 1, main = "QDA")





####
#LDA
####

lda.planet<- lda(P_H~P_P+S_T+P_D+P_PN+P_A+P_D_E+P_F+P_T_E+S_R_E+S_L+P_R+P_M, data=Planets_dataset, subset=Planets_dataset_train)

#X11(width=60, height=60)
#partimat(P_H~S_T+S_L+P_T_E,data=Planets_dataset[Planets_dataset_train,],method="lda",nplots.vert=4)


plot(lda.planet)


lda.prob<-data.frame(predict(lda.planet,Planets_dataset[-Planets_dataset_train,],type = "response"))
lda.prob<-lda.prob["class"]

lda_fin<-data.frame(lda.prob,stringsAsFactors = TRUE)
lda_fin["Test"]<-as.factor(Planets_dataset[-Planets_dataset_train,12])

colnames(lda_fin)<-c("Predict","Test")

caret::confusionMatrix(table(lda_fin))

fourfoldplot(table(lda_fin), color = c("red","darkgreen"),conf.level = 0, margin = 1, main = "LDA")




##########################
#Logistic classitification
##########################



model <- glm(P_H~P_P+S_T+P_D+P_PN+P_A+P_D_E+P_F+P_T_E+S_R_E+S_L+P_R+P_M,family=binomial(link='logit'),data=Planets_dataset[,-c(1)])

summary(model)

logistic.prob<-data.frame(predict(model ,Planets_dataset[-Planets_dataset_train,],type = "response"))
colnames(logistic.prob)<-c("P")
logistic.prob<- data.frame(ifelse(logistic.prob > 0.5, "True", "False"))
logistic.prob["T"]<-as.factor(Planets_dataset[-Planets_dataset_train,12])

fourfoldplot(table(logistic.prob), color = c("red","darkgreen"),conf.level = 0, margin = 1, main = "Logistic")


###############
#Auxiliary code
#It takes an excel file where a predicted/test values for 10 runs are reported and plot the confusion matrix
###############

pca.planet <- prcomp(pca.train[,2:14], center = TRUE,scale. = TRUE)
pca3d(pca.planet,group= pca.train[,12])


Conf_matrix_dec_tree <- read_excel("Final_data/Strumenti/Conf_matrix_QDA_boolean.xlsx")

caret::confusionMatrix(table(Conf_matrix_dec_tree))

fourfoldplot(table(Conf_matrix_dec_tree), color = c("red","darkgreen"),conf.level = 0, margin = 1)




###################################################
#Check on the radomization of non habitable planets
###################################################


phl_exoplanet_FULLC <- data.frame(read_excel("phl_exoplanet_catalog_RENAMED.xlsx"),stringsAsFactors = FALSE)
Planet_not_habitable_FULLC<-subset(phl_exoplanet_FULLC,P_H="False")
levels(Planet_not_habitable_FULLC$P_H) <- c("False","True")
levels(Planets_dataset$P_H) <- c("False","True")
Planet_not_habitable_FULLC$Full<-c("True")
Planets_dataset$Full<-c("False")
Planet_not_habitable<-subset(Planets_dataset,P_H="False")
check_final<-rbind(Planet_not_habitable,Planet_not_habitable_FULLC)
ggdensity(check_final,x="P_T_E",rug = TRUE, color = "Full",fill = "Full" )+theme_bw()

####################################
#Check on the variable distributions
####################################

Planet_not_habitable<-subset(Planets_dataset,P_H="False")
Planet_habitable<-subset(Planets_dataset,P_H="True")

ggdensity(Planets_dataset,x="S_R_E",rug = TRUE, color = "P_H",fill = "P_H" )+theme_bw()


ggdensity(Planet_not_habitable$S_LUMINOSITY)
