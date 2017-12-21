## FIle : Random Forest:
# Author ArctoScience
# Date : 18/12 
source("decisionTree.R")

bagging <- function(data,target,numBootstrap = 100, tailleSubspace = floor(sqrt(ncol(data) - 1)), impurityMethod="entropy",maxDepth=2, minLeafSize = 1, impurityThreshold = 0.2){
  require(rlist)
  trees <- list()
  for (i in 1:numBootstrap) {
    
    sampleIDs <- sample(nrow(data),replace = TRUE)
    bootstrap <- data
    bootstrap[1,] <- data[sampleIDs[1],]
    for (i in sampleIDs[2:length(sampleIDs)]) {
      bootstrap[i,] <- data[sampleIDs[i],]
    }
    tempTree = createDecisionTreeModel(bootstrap,target,impurityThreshold,maxDepth,minLeafSize,tailleSubspace = tailleSubspace)
    trees <- list.append(trees,tempTree)
  }
  return(trees)
}

predictFromForest <- function(forest,input){
  predictions <-  Reduce("c",lapply(forest,function(t) {
   l <- predictFromDecisionTree(t, input)
   return(names(l[which.max(l)])[1])
   }))
  repartitionVotes <- table(predictions) / sum(table(predictions))
  return(repartitionVotes)
}
vectorizedPredictFromForest <- function(forest,input){
  predictionsbyx <- rep("none",nrow(input))
  for(i in 1:nrow(input)){
    bob <- predictFromForest(forest,input[i,])
    predictionsbyx[i] <-names(bob)[which.max(bob)][1]
  }
  predictionsbyx
}

parallelBagging <- function(numCore = detectCores(), data,target,input,numBootstrap = 100, tailleSubspace = floor(sqrt(ncol(data) - 1)), impurityMethod="entropy", maxDepth=2, minLeafSize = 1, impurityThreshold = 0.2){
  require(parallel)
  cl <- makeCluster(numCore)
  clusterExport(cl, list("data","target","input","tailleSubspace","impurityMethod", "maxDepth","minLeafSize","impurityThreshold"),envir = environment())
  clusterEvalQ(cl,source("decisionTree.R"))
  res <- parLapply(cl, 1:numBootstrap, fun = function(i) {
    sampleIDs <- sample(nrow(data),replace = TRUE)
    bootstrap <- data
    bootstrap[1,] <- data[sampleIDs[1],]
    for (i in sampleIDs[2:length(sampleIDs)]) {
      bootstrap[i,] <- data[sampleIDs[i],]
    }
    tempTree = createDecisionTreeModel(bootstrap,target,impurityThreshold = 0.1,maxDepth = 2,minLeafSize = 5,tailleSubspace = tailleSubspace)
    l <- predictFromDecisionTree(tempTree,input)
    return(names(l[which.max(l)]))
  })
  
  predictions <- Reduce("c",res)
  repartitionVotes <- table(predictions) / sum(table(predictions))
  # Il peut y avoir des ex-aequo, on renvoit donc une liste
  return(repartitionVotes)
}
