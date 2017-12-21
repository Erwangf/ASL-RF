## FIle : Random Forest:
# Author ArctoScience
# Date : 18/12 
source("decisionTree.R")

bagging <- function(data,target,input,numBootstrap = 100, tailleSubspace = floor(sqrt(ncol(data) - 1)), impurityMethod="entropy",
                    maxDepth=300, minLeafSize = 1, impurityThreshold = 0.2){
  
  predictions <- rep(0,numBootstrap)
  for (i in 1:numBootstrap) {
    
    sampleIDs <- sample(data,nrow(data),replace = TRUE)
    
    tempTree <- createDecisionTreeModel(data[sampleIDs,],target,impurityMethod,maxDepth,minLeafSize,impurityThreshold,tailleSubspace)
    
    predictions[i] <- predictFromDecisionTree(tempTree,input)
    
  }

  # votesMajoritaires <- which.max(table(predictions))
  probaGlobal = apply(predictions,2,mean)
  
  return(probaGlobal)
}


createRFModel <- function(data,target,input,numBootstrap = 100, tailleSubspace = floor(sqrt(ncol(data) - 1)), impurityMethod="entropy",
                        maxDepth=300, minLeafSize = 1, impurityThreshold = 0.2){
  
  predictions <- rep(0,numBootstrap)
  for (i in 1:numBootstrap) {
    
    sampleIDs <- sample(data,nrow(data),replace = TRUE)
    
    tempTree <- createDecisionTreeModel(data[sampleIDs,],target,impurityMethod,maxDepth,minLeafSize,impurityThreshold,tailleSubspace)
    
    predictions[i] <- predictFromDecisionTree(tempTree,input)
    
  }
  
  # votesMajoritaires <- which.max(table(predictions))
  probaGlobal = apply(predictions,2,mean)
  
  return(probaGlobal)
}

predictWithRFModel <- function(model,input){
  predictions <- lapply(model,function(dt){
    predictFromDecisionTree(dt,input)
  })
  
  # agglomerer
  globalProba <- apply(prediction,2,mean)
  
  return(globalProba)
  
}
