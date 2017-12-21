source("./decisionTree.R")
source("./RandomForest2.R")
# 
# #### Tools ####
errRate = function(pred,truth){
  c = table(pred,truth)
  return(1 - sum(diag(c))/sum(c))
}
# 
# 
# #### Tests ####
# 
# # stupid tree model + predictFromDecisionTree
# stupidData = data.frame(X1=c(17,17,18),X2=c(2,3,3),X3=c(FALSE,TRUE,TRUE))
# 
# # quantitative data
# qData = data.frame(V1=c(10,18,13,14,19,22,26,39,22,40), 
#                    V2=c(100,165,140,168,199,145,155,111,194,151),
#                    V3=c(0,0,0,0.2,0.8,1,0.4,0.5,0.1,0.1),
#                    V4=c("A","B","A","A","B","A","A","B","B","A"))
# 
# # qualitative data
# fData = data.frame(V1 = c("A","A","B","B","C","D","E"),
#                    V2 = c("1","2","3","4","3","2","1"),
#                    V3 = c("W","X","W","X","W","W","W"),
#                    V4 = c(T,F,F,F,T,T,F))
# # config
# qConfig = createConfig(qData,"V4",impurityMethod = "entropy",maxDepth = 3,minLeafSize=2,impurityThreshold=0.1)
# 
# # model creation
# stupidModel = createStupidTreeModel(stupidData)
# apply(stupidData,1, function(row){predictFromDecisionTree(stupidModel,row)}) # ==> FALSE TRUE TRUE
# 
# # classification entropy
# classificationEntropy(data.frame(X1=c(1,1,0,0)),target="X1") # ==> 1
# classificationEntropy(data.frame(X1=c(1,1,1,1)),target="X1") # ==> 0
# classificationEntropy(data.frame(X1=c(1,0,0,0)),target="X1") # ==> ~ 0.8113
# classificationEntropy(data.frame(X1=c(1,1,1,0)),target="X1") # ==> ~ 0.8113
# 
# # splitting dataset
# splitNumVar(stupidData,"X1","X3") #  17.3333333
# splitNumVar(qData,"V3","V4") # 0.31
# splitNumVar(qData,"V2","V4") # 152.8
# splitFacVar(fData,"V3","V4") # W
# splitFacVar(fData,"V1","V4") # B
# 
# # Decision tree : quantitative variables, classification
# createDecisionTreeModel(stupidData,"X3")
# createDecisionTreeModel(qData,"V4")
# 
# qd = createDecisionTreeModel(qData,"V4")
# result = as.vector(apply(qData,1,function(i){predictFromDecisionTree(qd,i)}))
# 
# errRate(result,qData$V4)

# Cross validation

# https://stats.stackexchange.com/questions/61090/how-to-split-a-data-set-to-do-10-fold-cross-validation Jake Drew code here
data <- iris[sample(nrow(iris)),]
#Create 10 equally size folds
folds <- cut(seq(1,nrow(data)),breaks=10,labels = FALSE)
#Perform 10 fold cross validation
errorRF <- 0
errorsimple <- 0
for (i in 1:10) {
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds == i,arr.ind = TRUE)
  test <- data[testIndexes, ]
  train <- data[-testIndexes, ]
  #Random Forest
  forest <- bagging(data = train, target = "Species",maxDepth = 4,numBootstrap = 200, tailleSubspace = 4,minLeafSize = 1)
  res <- vectorizedPredictFromForest(forest,test)
  errorRF <- errorRF + errRate(res,test$Species)
  
  #Simple DT
  simpleDT = createDecisionTreeModel(train,"Species",maxDepth = 4,minLeafSize = 1)
  resultIris = t(apply(test,1,function(i){predictFromDecisionTree(simpleDT,i)}))
  prediction = apply(resultIris,1,function(l){names(l[which.max(l)])})
  errorsimple <- errorsimple + errRate(prediction,test$Species)
}
errorRF <- errorRF/10
errorsimple <- errorsimple/10


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

data <- VisaPremier[sample(nrow(VisaPremier)),]
#Create 10 equally size folds
folds <- cut(seq(1,nrow(data)),breaks=10,labels = FALSE)


testIndexes <- which(folds == 1,arr.ind = TRUE)
test <- data[testIndexes, ]
train <- data[-testIndexes, ]
#Random Forest
forest <- bagging(data = train, target = cible,maxDepth = 4,numBootstrap = 10, tailleSubspace = 6,minLeafSize = 1)
res <- vectorizedPredictFromForest(forest,test)
errorRF <- 0
errorRF <- errorRF + errRate(res,test$Species)

#Perform 10 fold cross validation
errorRF <- 0
errorsimple <- 0
for (i in 1:10) {
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds == i,arr.ind = TRUE)
  test <- data[testIndexes, ]
  train <- data[-testIndexes, ]
  #Random Forest
  forest <- bagging(data = train, target = cible,maxDepth = 4,numBootstrap = 200, tailleSubspace = 4,minLeafSize = 1)
  res <- vectorizedPredictFromForest(forest,test)
  errorRF <- errorRF + errRate(res,test$Species)
  
  #Simple DT
  simpleDT = createDecisionTreeModel(train,"Species",maxDepth = 4,minLeafSize = 1)
  resultIris = t(apply(test,1,function(i){predictFromDecisionTree(simpleDT,i)}))
  prediction = apply(resultIris,1,function(l){names(l[which.max(l)])})
  errorsimple <- errorsimple + errRate(prediction,test$Species)
}
errorRF <- errorRF/10
errorsimple <- errorsimple/10

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
# #### Tools ####
# errRate = function(pred,truth){
#   c = table(pred,truth)
#   return(1 - sum(diag(c))/sum(c))
# }
# 
# # test sur iris
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


