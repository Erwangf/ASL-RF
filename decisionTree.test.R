source("./decisionTree.R")


#### Tests ####

# stupid tree model + predictFromDecisionTree
stupidData = data.frame(X1=c(17,17,18),X2=c(2,3,3),X3=c(FALSE,TRUE,TRUE))

# quantitative data
qData = data.frame(V1=c(10,18,13,14,19,22,26,39,22,40), 
                   V2=c(100,165,140,168,199,145,155,111,194,151),
                   V3=c(0,0,0,0.2,0.8,1,0.4,0.5,0.1,0.1),
                   V4=c("A","B","A","A","B","A","A","B","B","A"))

# model creation
stupidModel = createStupidTreeModel(stupidData)
apply(stupidData,1, function(row){predictFromDecisionTree(stupidModel,row)}) # ==> FALSE TRUE TRUE

# classification entropy
classificationEntropy(data.frame(X1=c(1,1,0,0)),target="X1") # ==> 1
classificationEntropy(data.frame(X1=c(1,1,1,1)),target="X1") # ==> 0
classificationEntropy(data.frame(X1=c(1,0,0,0)),target="X1") # ==> ~ 0.8113
classificationEntropy(data.frame(X1=c(1,1,1,0)),target="X1") # ==> ~ 0.8113

# splitting dataset
splitDataset(stupidData,"X1",classes=c("POSITIVE","NEGATIVE")) # ==> L = 1,2 R = 3, X1 >= 17.3333333
splitDataset(data.frame(X1=c("A","A","B","C")),"X1",classes=c("POSITIVE","NEGATIVE")) # ==> random separation
splitDataset(qData,"V3",c("A","B")) # V3>=0.31
splitDataset(qData,"V2",c("A","B")) # V2>=152.8
splitDataset(qData,"V4",c("A","B")) # V4=='A'

# Decision tree : quantitative variables, classification
createDecisionTreeModel(stupidData,"X3")
createDecisionTreeModel(qData,"V4")