source("./decisionTree.R")

# 
# #### Tools ####
errRate = function(pred,truth){
  c = table(pred,truth)
  return(1 - sum(diag(c))/sum(c))
}


#### Tests ####

# stupid tree model + predictFromDecisionTree
stupidData = data.frame(X1=c(17,17,18),X2=c(2,3,3),X3=c(FALSE,TRUE,TRUE))

# quantitative data
qData = data.frame(V1=c(10,18,13,14,19,22,26,39,22,40),
                   V2=c(100,165,140,168,199,145,155,111,194,151),
                   V3=c(0,0,0,0.2,0.8,1,0.4,0.5,0.1,0.1),
                   V4=c("A","B","A","A","B","A","A","B","B","A"))

# qualitative data
fData = data.frame(V1 = c("A","A","B","B","C","D","E"),
                   V2 = c("1","2","3","4","3","2","1"),
                   V3 = c("W","X","W","X","W","W","W"),
                   V4 = c(T,F,F,F,T,T,F))
# config
qConfig = createConfig(qData,"V4",impurityMethod = "entropy",maxDepth = 3,minLeafSize=2,impurityThreshold=0.1)

# model creation
stupidModel = createStupidTreeModel(stupidData)
apply(stupidData,1, function(row){predictFromDecisionTree(stupidModel,row)}) # ==> FALSE TRUE TRUE

# classification entropy
classificationEntropy(data.frame(X1=c(1,1,0,0)),target="X1") # ==> 1
classificationEntropy(data.frame(X1=c(1,1,1,1)),target="X1") # ==> 0
classificationEntropy(data.frame(X1=c(1,0,0,0)),target="X1") # ==> ~ 0.8113
classificationEntropy(data.frame(X1=c(1,1,1,0)),target="X1") # ==> ~ 0.8113

# splitting dataset
splitNumVar(stupidData,"X1","X3") #  17.3333333
splitNumVar(qData,"V3","V4") # 0.31
splitNumVar(qData,"V2","V4") # 152.8
splitFacVar(fData,"V3","V4") # W
splitFacVar(fData,"V1","V4") # B

# Decision tree : quantitative variables, classification
createDecisionTreeModel(stupidData,"X3")
createDecisionTreeModel(qData,"V4")

qd = createDecisionTreeModel(qData,"V4")
result = as.vector(apply(qData,1,function(i){predictFromDecisionTree(qd,i)}))

errRate(result,qData$V4)
# # test sur iris
iris
par(mfrow=c(2,1))
plot(iris$Sepal.Length, iris$Sepal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)], main = "Iris Data")
plot(iris$Petal.Length, iris$Petal.Width, pch=21, bg=c("red","green3","blue")[unclass(iris$Species)], main = "Iris Data")

irisDT = createDecisionTreeModel(iris,"Species",impurityThreshold = 0.1,maxDepth = 2,minLeafSize = 5)
resultIris = t(apply(iris,1,function(i){predictFromDecisionTree(irisDT,i)}))
predictionIris = apply(resultIris,1,function(l){names(l[which.max(l)])})
errRate(predictionIris,iris$Species)

# test sur des données qualitatives
bankrupt <- read.csv("./Qualitative_Bankruptcy/Qualitative_Bankruptcy.data.txt",header = F,sep = ",")
bankruptDT <- createDecisionTreeModel(bankrupt,"V7")
resultBankrupt = t(apply(bankrupt,1,function(i){predictFromDecisionTree(bankruptDT,i)}))
predictionBankrupt = apply(resultBankrupt,1,function(l){names(l[which.max(l)])})
errRate(predictionBankrupt,bankrupt$V7)

library(rpart)
library(rpart.plot)
dt = rpart(data = iris,formula = Species ~.)
rpart.plot(dt)
errRate(apply(predict(dt,iris),1,function(l){names(l[which.max(l)])}),iris$Species)
