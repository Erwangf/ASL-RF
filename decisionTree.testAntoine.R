setwd("D:/Bureau/DM/Advanced supervised learning/Projet/ASL-RF")
source("./decisionTree.R")
source("./RandomForest2.R")
# 
# #### Tools ####
errRate = function(pred,truth){
  c = table(pred,truth)
  return(1 - sum(diag(c))/sum(c))
}

# Cross validation IRIS ####
# 
# # https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation Jake Drew code here
# data <- iris[sample(nrow(iris)),]
# #Create 10 equally size folds
# folds <- cut(seq(1,nrow(data)),breaks=10,labels = FALSE)
# #Perform 10 fold cross validation
# errorRF <- 0
# errorsimple <- 0
# for (i in 1:10) {
#   #Segement your data by fold using the which() function 
#   testIndexes <- which(folds == i,arr.ind = TRUE)
#   test <- data[testIndexes, ]
#   train <- data[-testIndexes, ]
#   #Random Forest
#   forest <- bagging(data = train, target = "Species",maxDepth = 4,numBootstrap = 200, tailleSubspace = 4,minLeafSize = 1)
#   res <- vectorizedPredictFromForest(forest,test)
#   errorRF <- errorRF + errRate(res,test$Species)
#   
#   #Simple DT
#   simpleDT = createDecisionTreeModel(train,"Species",maxDepth = 4,minLeafSize = 1)
#   resultIris = t(apply(test,1,function(i){predictFromDecisionTree(simpleDT,i)}))
#   prediction = apply(resultIris,1,function(l){names(l[which.max(l)])})
#   errorsimple <- errorsimple + errRate(prediction,test$Species)
# }
# errorRF <- errorRF/10
# errorsimple <- errorsimple/10

# Cross validation VisaPremier ####
#Prétraitements
library(readr)
VisaPremier <- read.delim("D:/Bureau/DM/Advanced supervised learning/Projet/ASL-RF/VisaPremier.txt")
VisaPremier <- VisaPremier[,-1]
VisaPremier <- VisaPremier[,-which(colnames(VisaPremier) == "sexer")]
VisaPremier <- VisaPremier[,-which(colnames(VisaPremier) == "cartevpr")]
which.max(VisaPremier$avtscpte)
VisaPremier[-2,]
VisaPremier$ptvente <- as.factor(VisaPremier$ptvente)

VisaPremier$nbpaiecb <- as.numeric(VisaPremier$nbpaiecb)
VisaPremier$agemvt <- as.numeric(VisaPremier$agemvt)

int <- which(sapply(VisaPremier, class) == "integer")
for (i in int) {
  VisaPremier[,i] <- as.numeric(VisaPremier[,i])
}
numVisaPremier <- VisaPremier[,which(sapply(VisaPremier, class) == "numeric")]
zero <- apply(numVisaPremier,2,function(x) sum(x == 0)) / nrow(numVisaPremier)
which(zero>0.9)
# nbimpaye  mtrejet nbeparlt mteparlt nbeparte mteparte    nbbon    mtbon
VisaPremier <- VisaPremier[,-which(colnames(VisaPremier) == "nbimpaye")]
VisaPremier <- VisaPremier[,-which(colnames(VisaPremier) == "mtrejet")]
VisaPremier <- VisaPremier[,-which(colnames(VisaPremier) == "nbeparlt")]
VisaPremier <- VisaPremier[,-which(colnames(VisaPremier) == "mteparlt")]
VisaPremier <- VisaPremier[,-which(colnames(VisaPremier) == "nbeparte")]
VisaPremier <- VisaPremier[,-which(colnames(VisaPremier) == "mteparte")]
VisaPremier <- VisaPremier[,-which(colnames(VisaPremier) == "nbbon")]
VisaPremier <- VisaPremier[,-which(colnames(VisaPremier) == "mtbon")]
VisaPremier$cartevp
cible = "cartevp"

table(VisaPremier$cartevp)
rm(numVisaPremier)
rm(zero)

#######Fin prétraitements

data <- VisaPremier[sample(nrow(VisaPremier)),]
#Create 10 equally size folds
folds <- cut(seq(1,nrow(data)),breaks=10,labels = FALSE)


testIndexes <- which(folds == 1,arr.ind = TRUE)
test <- data[testIndexes, ]
train <- data[-testIndexes, ]

# bobi <- randomForest::randomForest(cartevp ~ .,train)
# bo <- predict(bobi, test)
# error <- errRate(bo,test$cartevp)
#Simple Tree for early testing
# simpleDT = createDecisionTreeModel(train,cible,maxDepth = 15,minLeafSize = 1)
# # debug(predictFromDecisionTree)
# resultIris = t(apply(test,1,function(i){predictFromDecisionTree(simpleDT,i)}))
# prediction = apply(resultIris,1,function(l){names(l[which.max(l)])})
# error <- errRate(prediction,test$cartevp)

#Simple Random Forest for early testing
forest <- bagging(data = train, target = cible,maxDepth = 4,numBootstrap = 10, tailleSubspace = 6,minLeafSize = 1)
res <- vectorizedPredictFromForest(forest,test)
errorRF <- 0
errorRF <- errorRF + errRate(res,test$cartevp)

train[,cible]
#Perform 10 fold cross validation
errorRF <- 0
errorsimple <- 0
error <- 0
for (i in 1:10) {
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds == i,arr.ind = TRUE)
  test <- data[testIndexes, ]
  train <- data[-testIndexes, ]
  #Random Forest
  # forest <- bagging(data = train, target = cible,maxDepth = 6,numBootstrap = 100, tailleSubspace = 6,minLeafSize = 1)
  # res <- vectorizedPredictFromForest(forest,test)
  # errorRF <- errorRF + errRate(res,test$cartevp)
  # 
  # #Simple DT
  # simpleDT = createDecisionTreeModel(train,cible,maxDepth = 15,minLeafSize = 1)
  # resultIris = t(apply(test,1,function(i){predictFromDecisionTree(simpleDT,i)}))
  # prediction = apply(resultIris,1,function(l){names(l[which.max(l)])})
  # errorsimple <- errorsimple + errRate(prediction,test$cartevp)
  bobi <- randomForest::randomForest(cartevp ~ .,train)
  bo <- predict(bobi, test)
  error <- errRate(bo,test$cartevp)
}
errorRF <- errorRF/10
errorsimple <- errorsimple/10
error <- error/10

summary(data)
# forest <- bagging(data = iris[idtrain,], target = "Species",numBootstrap = 200, tailleSubspace = 3)
# test <- iris[-idtrain,]
# res <- vectorizedPredictFromForest(forest,test)
# errRate(res,test$Species)
# 
# # test sur iris
# simpleDT = createDecisionTreeModel(iris[idtrain,],"Species",maxDepth = 4,minLeafSize = 1)
# resultIris = t(apply(test,1,function(i){predictFromDecisionTree(simpleDT,i)}))
# prediction = apply(resultIris,1,function(l){names(l[which.max(l)])})
# errRate(prediction,test$Species)



# a <- parallelBagging(data = iris, target = "Species",input = iris[50,],numBootstrap = 100, tailleSubspace = 4)
# 
# Rprof()
# invisible(parallelBagging(data = iris, target = "Species",input = iris[50,],numBootstrap = 100, tailleSubspace = 4))
# Rprof(NULL)
# summaryRprof()
# 
# library(randomForest)
# comparaison <- microbenchmark::microbenchmark(bagging(data = iris, target = "Species",input = iris[50,],numBootstrap = 200),
#                                parallelBagging(data = iris, target = "Species",input = iris[50,],numBootstrap = 200),
#                                rf1 <- randomForest(Species ~ ., iris, ntree=200, norm.votes=FALSE),
#                                times = 5
# )
# print(comparaison)

# # Plots
iris
par(mfrow=c(2,1))
plot(iris$Sepal.Length, iris$Sepal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)], main = "Iris Data")
plot(iris$Petal.Length, iris$Petal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)], main = "Iris Data")
# 
# irisDT = createDecisionTreeModel(iris,"Species",impurityThreshold = 0.1,maxDepth = 2,minLeafSize = 5)
# resultIris = t(apply(iris,1,function(i){predictFromDecisionTree(irisDT,i)}))
# prediction = apply(resultIris,1,function(l){names(l[which.max(l)])})
# errRate(prediction,iris$Species)


# comparing with rpart
# library(rpart)
# library(rpart.plot)
# dt = rpart(data = iris,formula = Species ~.)
# rpart.plot(dt)
# 
# errRate(apply(predict(dt,iris),1,function(l){names(l[which.max(l)])}),iris$Species)


