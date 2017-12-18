## FIle : Random Forest:
# Author ArctoScience
# Date : 18/12 
source("decisionTree.R")

bagging <- function(data,target,input,numBootstrap = 100, tailleSubspace = floor(sqrt(ncol(data) - 1)), impurityMethod="entropy",maxDepth=300, minLeafSize = 1, impurityThreshold = 0.2){
  
  predictions <- rep(0,numBootstrap)
  for (i in 1:numBootstrap) {
    
    sampleIDs <- sample(data,nrow(data),replace = TRUE)
    
    tempTree <- createDecisionTreeModel(data[sampleIDs,],target,impurityMethod,maxDepth,minLeafSize,impurityThreshold,tailleSubspace)
    
    predictions[i] <- predictFromDecisionTree(tempTree,input)
    
  }

  votesMajoritaires <- which.max(table(predictions))
  # Il peut y avoir des ex-aequo, on renvoit donc une liste
  return(names(votesMajoritaires))
}


