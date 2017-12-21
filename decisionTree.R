## FIle : Stupid decision tree :
# Author ArctoScience
# Date : 18/12 


# exemple de retour : TRUE if 
# var "X1" >= 18 
# or 
# var"X1" <18 et var X2>2
# pour l'instant, param?tre inutilis?s
createStupidTreeModel <- function(data,target,
                                  impurityMethod="entropy",
                                  maxDepth=300,
                                  minLeafSize = 1,
                                  impurityThreshold=0.2){
  
  
  a = list(cond = ">=18", var = "X1",
           L = list(cond = ">2", var = "X2",
                    L = list(V = FALSE),
                    R = list(V = TRUE)), 
           R = list(V = TRUE))
  return(a)
}

#Fonction générale
#Input : data :dataframe (todo test dataframe)
# target variable cible
# impurityMethod="entropy" then gini and third??
# maxDepth : profondeur max
# minLeafSize : taille d'une feuille minimum (conditon d'arret)
# impurityThreshold : valeur d'impuret? minimum
createDecisionTreeModel <- function(data,target,
                                    impurityMethod="entropy",
                                    maxDepth=300,
                                    minLeafSize = 1,
                                    impurityThreshold=0.2, tailleSubspace = (ncol(data)-1)){
  variables <- names(data)
  availableVars <- variables[variables != target]
  targetClasses <- as.vector(t(unique(data[target])))
  config = list(maxDepth=maxDepth,
                minLeafSize=minLeafSize,
                impurityThreshold = impurityThreshold, 
                impurityMethod = impurityMethod, 
                target=target,
                targetClasses = targetClasses)
  
  
  
  rootNode = expandNode(node=list(depth=0),
                        data = data,
                        availableVars = availableVars , 
                        config = config,
                        tailleSubspace)
  return(rootNode)
  
}


predictFromDecisionTree <-function(decisionTreeModel, item){
  node = decisionTreeModel
  currentDepth = 0
  while(TRUE){
    if(is.null(node$L) && is.null(node$R)){
      return(node$V)
    }
    currentDepth = currentDepth + 1
    # application de la condition du noeud :
    cond = paste("item['",node$var,"']",node$cond,sep="")
    goToRight = eval(parse(text=cond))
    # print(paste("Depth : ",currentDepth,"condition =",cond," = ",goToRight))
    if(goToRight){
      node = node$R
    } else {
      node = node$L
    }
  }
}

#### Tools functions ####

# Entropy of q classes
# target : number (of the target column in data) or string
classificationEntropy <- function(data,target){
  targetCol = t(as.vector(data[target]))
  n = length(targetCol)
  P = table(targetCol)
  nodeEntropy = 0
  for (l in 1:length(P)) {
    if(P[l]!=0){
      nodeEntropy = nodeEntropy - (P[l]/n)*log2(P[l]/n)
    }
  }
  
  return(as.vector(nodeEntropy))
}
simpleClassificationEntropy <- function(vector){
  n = length(vector)
  P = table(vector)
  nodeEntropy = 0
  for (l in 1:length(P)) {
    if(P[l]!=0){
      nodeEntropy = nodeEntropy - (P[l]/n)*log2(P[l]/n)
    }
  }
  
  return(as.vector(nodeEntropy))
}

splitNumVar <- function(data,splitVariable,target,minLeafSize=1){
  classVector <- data[,target][order(data[,splitVariable])]
  inpurityMin <- Inf
  indiceSep <- NULL
  for (i in 1:(length(classVector) - 1)) {
    if (classVector[i + 1] != classVector[i])
    {
      left = classVector[1:i]
      right = classVector[i + 1:length(classVector)]
      inpurity = (length(left) * simpleClassificationEntropy(left) + length(right) * simpleClassificationEntropy(right)) / nrow(iris)
      if (inpurityMin > inpurity){
        inpurityMin <- inpurity
        indiceSep <- i
      }
    }
  }

  quantVector <- data[,splitVariable][order(data[,splitVariable])]
  splitPoint <- (quantVector[indiceSep] + quantVector[indiceSep + 1] ) / 2
  return(splitPoint)
}

splitFacVar <- function(data,splitVariable,target,minLeafSize=1){
  
  modalities = as.vector(t(unique(data[splitVariable])))
  
  lowestInpurity = NULL
  lowestInpurityLeft = NULL
  lowestInpurityRight = NULL
  lowestInpurityCond = NULL
  modIndex = 1
  
  # trouver la meilleure modalité
  for(i in 1:length(modalities)) {
    modality = modalities[i]
    
    left = data[data[splitVariable] != modality,]
    right = data[data[splitVariable] == modality,]
    cond= paste("=='",modality,"'",sep = "")
    
    # calcul de l'impureté
    inpurity = ( nrow(left) * classificationEntropy(left,target) + nrow(right) * classificationEntropy(right,target) ) / nrow(data)
    
    if((is.null(lowestInpurity) || inpurity < lowestInpurity) && nrow(left) >= minLeafSize && nrow(right) >= minLeafSize) {
      lowestInpurity = inpurity
      lowestInpurityLeft = left
      lowestInpurityRight = right
      modIndex = i
    }
  }
  return(modalities[modIndex])
}

leafValue <- function(data,target,classes){
  # currently, if use a (not always efficient) custom table + names
  # res <- names(which.max(table(data[target])))
  # return(res)
  
  proba = sapply(classes,function(e){mean(data[target]==e)})
  
  return(proba)
}


expandNode <- function(node,data,availableVarsDefault,config,tailleSubspace){
  # print(paste("Depth = ",node$depth, "/",config$maxDepth))
  targetColumn = data[,config$target]
  # depth control
  if(node$depth>=config$maxDepth){
    # print("Maximal Depth reached")
    return(list(V=leafValue(data,config$target,config$targetClasses)))
  }
  
  availableVars <- availableVarsDefault[sample(tailleSubspace)]

  
  availableClasses = unique(targetColumn)
  
  # check if the node is pure
  if(length(availableClasses)==1){
    return(list(V=leafValue(data,config$target,config$targetClasses)))
  }
  
  impurity = classificationEntropy(data,config$target)
  # impurity threshold
  if(impurity<config$impurityThreshold){
    # print(paste("Current Entropy = ",currentEntropy," => no expansion"))
    return(list(V=leafValue(data,config$target,config$targetClasses)))
  }
  
  split = NULL
  varIndex = 0
  while(varIndex<length(availableVars)){
    varIndex = varIndex + 1
    splitVar = availableVars[varIndex]
    newSplit = list(var=splitVar)
    
    varType = class(as.vector(t(data[splitVar]))[1])
    if(varType == "numeric"){
      splitPosition = splitNumVar(data,splitVar,config$target,config$minLeafSize)
      newSplit$cond = paste(">=",splitPosition)
      newSplit$L = data[data[splitVar]<splitPosition,]
      newSplit$R = data[data[splitVar]>=splitPosition,]
    } else {
      splitMod = splitFacVar(data,splitVar,config$target,config$minLeafSize)
      newSplit$cond = paste("==",splitMod)
      newSplit$L = data[data[splitVar] != splitMod,]
      newSplit$R = data[data[splitVar] == splitMod,]
    }
    
    N = nrow(data)
    nL = nrow(newSplit$L)
    nR = nrow(newSplit$R)
    
    if(nL != 0 && nR != 0){ 
      
      # print(splitVar)
      # print(paste("nL = ",nL, " nR = ",nR, " N = ", N))
      
      # total impurity
      impurityL = classificationEntropy(newSplit$L,config$target)
      impurityR = classificationEntropy(newSplit$R,config$target)
      newImpurity = (nL/N) * impurityL + (nR/N) * impurityR
      # print(newImpurity)
      if(newImpurity<impurity && nL>=config$minLeafSize && nR >= config$minLeafSize ){
        split = newSplit
        impurity = newImpurity
      }
     
    }
  }
  if(!is.null(split)){
    node$cond = split$cond
    node$var = split$var
    node$L = expandNode(list(depth=node$depth+1),data=split$L,availableVarsDefault,config=config,tailleSubspace)
    node$R = expandNode(list(depth=node$depth+1),data=split$R,availableVarsDefault,config=config,tailleSubspace)
    return(node)
  } else {
    
    return(list(V=leafValue(data,config$target,config$targetClasses)))
    
  }
  
  
}



