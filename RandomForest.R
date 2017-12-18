## FIle : Random Forest:
# Author ArctoScience
# Date : 18/12 
source("decisionTree.R")

Bagging <- function(){
  
}

#Fonction générale
#Input : data :dataframe (todo test dataframe)
# target variable cible
# impurityMethod="entropy" then gini and third??
# maxDepth : profondeur max
# minLeafSize : taille d'une feuille minimum (conditon d'arret)
# impurityThreshold : valeur d'impureté minimum
createDecisionTreeModel <- function(data,target,
                                    impurityMethod="entropy",
                                    maxDepth=300,
                                    minLeafSize = 1,
                                    impurityThreshold=0.2){
  variables = names(data)
  availableVars = variables[variables!=target]
  config = list(maxDepth=maxDepth,
                minLeafSize=minLeafSize,
                impurityThreshold = impurityThreshold, 
                impurityMethod = impurityMethod, 
                target=target)
  
  
  
  rootNode = expandNode(node=list(depth=0),
                        data = data,
                        availableVars = availableVars , 
                        config = config)
  return(rootNode)
}
