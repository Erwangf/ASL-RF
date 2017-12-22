source("./decisionTree.R")

# #### Test file for Decision Tree functions ####
#
# In this file, we can test some parts of the decision tree implementation
# We first use synthetic data (qualitative and quantitative), and compare on classical data (iris & bankrupt).
# Finally, we compare with the rpart implementation


# #### Tools ####

# compute the generalization error between 2 vectors : prediction and truth
# ex :  errRate(c(1,0,0,0,1) , c(1,1,1,0,0) ) ==> 1/5 correct ==> 4/5 wrong ==> errRate = 0.8
errRate = function(pred,truth){
  c = table(pred,truth)
  return(1 - sum(diag(c))/sum(c))
}

# apply the predictViaDT function over a dataset, selecting for each line the
# class with the highest class probability
applyDTonDataset <- function(decisionTreeModel,dataset){
  result = t(apply(dataset,1,function(i){predictViaDT(decisionTreeModel,i)}))
  prediction = apply(result,1,function(l){names(l[which.max(l)])})
  return(prediction)
}


#### General Tests ####

# stupid tree model + predictFromDecisionTree
stupidData = data.frame(X1 = c(17,17,18),X2 = c(2,3,3),X3 = c(FALSE,TRUE,TRUE))

# quantitative data
qData = data.frame(V1 = c(10,18,13,14,19,22,26,39,22,40),
                   V2 = c(100,165,140,168,199,145,155,111,194,151),
                   V3 = c(0,0,0,0.2,0.8,1,0.4,0.5,0.1,0.1),
                   V4 = c("A","B","A","A","B","A","A","B","B","A"))

# qualitative data
fData = data.frame(V1 = c("A","A","B","B","C","D","E"),
                   V2 = c("1","2","3","4","3","2","1"),
                   V3 = c("W","X","W","X","W","W","W"),
                   V4 = c(T,F,F,F,T,T,F))
# config
qConfig = createConfig(qData,"V4",impurityMethod = "entropy",maxDepth = 3,minLeafSize=2,impurityThreshold=0.1)


# classification entropy
classificationEntropy(data.frame(X1 = c(1,1,0,0)),target = "X1") # ==> 1
classificationEntropy(data.frame(X1 = c(1,1,1,1)),target = "X1") # ==> 0
classificationEntropy(data.frame(X1 = c(1,0,0,0)),target = "X1") # ==> ~ 0.8113
classificationEntropy(data.frame(X1 = c(1,1,1,0)),target = "X1") # ==> ~ 0.8113

# splitting dataset
splitNumVar(stupidData,"X1","X3") #  17.3333333
splitNumVar(qData,"V3","V4") # 0
splitNumVar(qData,"V2","V4") # 105.5
splitFacVar(fData,"V3","V4") # W
splitFacVar(fData,"V1","V4") # B

# Decision tree : quantitative variables, classification
createDecisionTreeModel(stupidData,"X3")
createDecisionTreeModel(qData,"V4")

# #### Decision Tree tests ####

# qData
qd = createDecisionTreeModel(qData,"V4")
resultQd = t(apply(qData,1,function(i){predictViaDT(qd,i)}))
predictionQd = apply(resultQd,1,function(l){names(l[which.max(l)])})

errRate(predictionQd,qData$V4)

# iris dataset

par(mfrow = c(2,1))
plot(iris$Sepal.Length, iris$Sepal.Width, pch = 21, bg = c("red","green3","blue")[unclass(iris$Species)], main = "Iris Data")
plot(iris$Petal.Length, iris$Petal.Width, pch = 21, bg = c("red","green3","blue")[unclass(iris$Species)], main = "Iris Data")

irisDT = createDecisionTreeModel(iris,"Species",impurityThreshold = 0.1,maxDepth = 2,minLeafSize = 5)
predictionIris = applyDTonDataset(irisDT,iris)
errRate(predictionIris,iris$Species)

# test sur des données qualitatives
bankrupt <- read.csv("./Qualitative_Bankruptcy/Qualitative_Bankruptcy.data.txt",header = F,sep = ",")
bankruptDT <- createDecisionTreeModel(bankrupt,"V7")

predictionBankrupt = applyDTonDataset(bankruptDT,bankrupt)
errRate(predictionBankrupt,bankrupt$V7)


# #### rpart implementation ####
library(rpart)
library(rpart.plot)
dt = rpart(data = iris,formula = Species ~.)
rpart.plot(dt)
errRate(apply(predict(dt,iris),1,function(l){names(l[which.max(l)])}),iris$Species)
