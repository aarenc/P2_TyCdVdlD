
setwd("E:/R/tipologia_ciclo_de_vida_datos")

library(readr)
library(RcmdrMisc)
library(stats)
library(car)
library("IRdisplay")

whitewine_clean <- read.table("E:/R/tipologia_ciclo_de_vida_datos/wineQualityWhites.csv",sep=",",header=TRUE)

attach(whitewine_clean)

str(whitewine_clean)

whitewine_clean$quality = as.factor(whitewine_clean$quality)

str(whitewine_clean)

head(whitewine_clean)

anyNA(whitewine_clean)

summary(whitewine_clean)

hist(whitewine_clean$quality)

shapiro.test(whitewine_clean$quality)$p.value

library(MVN)
mardiaTest(whitewine_clean)

hzTest(whitewine_clean)

library(mvnormtest)
mshapiro.test(t(whitewine_clean))

RegModel.4 <- lm(quality~alcohol+density+fixed.acidity+free.sulfur.dioxide+pH+residual.sugar+sulphates+volatile.acidity, 
data=whitewine_clean)
summary(RegModel.4)

ncvTest(RegModel.4)

spreadLevelPlot(RegModel.4)

outlierTest(RegModel.4)

whitewine_clean[c(4746,2782,3308,254,446),]

dcook = cooks.distance(RegModel.4)

table(dcook>1)

which(dcook>1)

whitewine_clean[2782,]

library(MASS)
Robusto_RegModel.4 <- rlm(quality~alcohol+density+fixed.acidity+free.sulfur.dioxide+pH+residual.sugar+sulphates+volatile.acidity, 
data=whitewine_clean)
summary(Robusto_RegModel.4)

library(rpart)
library(rpart.plot)

porcentaje_75<- nrow(whitewine_clean)*0.75
mw_1 <- whitewine_clean[sample(nrow(whitewine_clean), porcentaje_75),]

nrow(mw_1)

summary(mw_1)

porcentaje_25 <- nrow(whitewine_clean)*0.25
mw_2 <- whitewine_clean[sample(nrow(whitewine_clean), porcentaje_25),]

nrow(mw_2)

summary(mw_2)

mw_1_tree <- rpart(quality ~ fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, method = "class", data = mw_1)

print(mw_1_tree)

rpart.plot(mw_1_tree,type =4,extra=6)

printcp(mw_1_tree)

plotcp(mw_1_tree)

mw_1_tree_poda<- prune(mw_1_tree, cp=0.029)

tablaw <- table(predict(mw_1_tree, newdata = mw_2, type = "class"), mw_2$quality)
tablaw

(sum(diag(tablaw)) / sum(tablaw))*100

vinos_normalizados <- as.data.frame(scale(whitewine_clean[,2:13]))

head(vinos_normalizados)

set.seed(30)

vinos_cluster <- kmeans(vinos_normalizados,centers=5)

names(vinos_cluster)

vinos_cluster$size

table(whitewine_clean$quality, vinos_cluster$cluster)

vinos_cluster$cluster

vinos_cluster$totss

vinos_cluster$betweenss

vinos_cluster$withinss

vinos_cluster$tot.withinss

vinos_cluster$centers

plot(vinos_normalizados, col =vinos_cluster$cluster,main="Matriz de dispersión.Datos normalizados")

opt_cluster<- kmeans(vinos_normalizados,centers=1)$tot.withinss

for(i in 2:20) opt_cluster[i]<- kmeans(vinos_normalizados,centers=i)$tot.withinss
    
plot(1:20,opt_cluster,type="b",xlab="K",ylab="tot.withinss",col="red", main="Kmeans. Busqueda de k óptimo")  


plot(vinos_normalizados$quality, vinos_normalizados$alcohol, col=vinos_cluster$cluster, xlab="Calidad", ylab="Alcohol", 
main="Clustering. Calidad vs Alcohol")

medias <- aggregate(whitewine_clean[,2:13],by=list(vinos_cluster$cluster),mean)
medias

whitewine_clean$quality = as.factor(whitewine_clean$quality)

Boxplot(alcohol~quality, data=whitewine_clean, id.method="y")

library(lattice)
xyplot(density ~ residual.sugar | quality, type="p", pch=16, 
  auto.key=list(border=TRUE), par.settings=simpleTheme(pch=16), 
  scales=list(x=list(relation='same'), y=list(relation='same')), 
  data=whitewine_clean)

densityPlot(sulphates~quality, data=whitewine_clean, bw="SJ", adjust=1, kernel="gaussian")

********************************************************************************************************************************

write.table(whitewine_clean, "E:/R/tipologia_ciclo_de_vida_datos/wineQualityWhites_2.csv", sep=";", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")

write.table(vinos_normalizados, "E:/R/tipologia_ciclo_de_vida_datos/vinos_normalizados.csv", sep=";", col.names=TRUE, row.names=FALSE, quote=TRUE, na="NA")
