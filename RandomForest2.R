## FIle : Random Forest:
# Author ArctoScience
# Date : 18/12 
source("decisionTree.R")

bagging <- function(data,target,input,numBootstrap = 100, tailleSubspace = 1, impurityMethod="entropy",maxDepth=2, minLeafSize = 1, impurityThreshold = 0.2){
  
  predictions <- c()
  for (i in 1:numBootstrap) {
    
    sampleIDs <- sample(nrow(data),replace = TRUE)
    bootstrap <- data
    bootstrap[1,] <- data[sampleIDs[1],]
    for (i in sampleIDs[2:length(sampleIDs)]) {
      bootstrap[i,] <- data[sampleIDs[i],]
    }
    tempTree = createDecisionTreeModel(bootstrap,target,impurityThreshold,maxDepth,minLeafSize,tailleSubspace = tailleSubspace)
    l <- predictFromDecisionTree(tempTree,input)
    predictions <- c(predictions,names(l[which.max(l)]))
  }

  repartitionVotes <- table(predictions) / sum(table(predictions))
  # Il peut y avoir des ex-aequo, on renvoit donc une liste
  return(repartitionVotes)
}


parallelBagging <- function(numCore = detectCores(), data,target,input,numBootstrap = 100, tailleSubspace = floor(sqrt(ncol(data) - 1)), impurityMethod="entropy", maxDepth=300, minLeafSize = 1, impurityThreshold = 0.2){
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
