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

Planets_dataset <- data.frame(read_excel("C:/Users/Marzio/Desktop/Planets/phl_exoplanet_catalogB.xlsx"))
row.names(Planets_dataset )<- Planets_dataset [,1]

tree.planet = rpart(P_HABITABLE~P_DISTANCE+P_PERIASTRON+P_APASTRON+P_DISTANCE_EFF+P_FLUX+P_TEMP_EQUIL+S_RADIUS_EST+S_LUMINOSITY,data=Planets_dataset,method="class")
rpart.plot(tree.planet,box.palette=c("red", "green"),digits=4)








planet.pca<-prcomp(Planets_dataset[,3:16], center = TRUE,scale. = TRUE)
heatmap(x = cor(Planets_dataset[,3:16]), col = palette, symm = TRUE, margins = c(10, 10), main = 'Planets',dist(Planets_dataset[,3:16],method = 'euclidean'))


autoplot(planet.pca,loadings = TRUE,loadings.size= 4, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 4,label=TRUE,label.vjust=1.5,loadings.label.repel=T,label.repel=T)



View(Planets_dataset)
summary(Planets_dataset)
pca(Planets_dataset)
