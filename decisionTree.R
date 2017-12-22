## FIle : Stupid decision tree :
# Author ArctoScience
# Date : 18/12 



# Crée un arbre de décision pour la classification
# Input : 
#     data : dataframe
#     target : variable cible
#     maxDepth : profondeur maximale (défaut : 30)
#     minLeafSize : taille minimale d'une feuille (défaut : 5)
#     impurityThreshold : impureté maximale d'une feuille (défaut : 0.1)
#     tailleSubspace : nombre de variables à utiliser (défaut : nombre de variables disponibles -1)
arbreGeneration <- function(data,target,
                                    maxDepth=30,
                                    minLeafSize = 5,
                                    impurityThreshold=0.1, 
                                    tailleSubspace = (ncol(data)-1)){
  variables <- names(data)
  availableVars <- variables[variables != target]
  targetClasses <- as.vector(t(unique(data[target])))
  config = list(maxDepth=maxDepth,
                minLeafSize=minLeafSize,
                impurityThreshold = impurityThreshold,
                target=target,
                targetClasses = targetClasses)
  
  
  # Un arbre de décision est constitué d'un noeud racine, menant à d'autres noeuds
  rootNode = expandNode(node = list(depth = 0),
                        data = data,
                        availableVars = availableVars , 
                        config = config,
                        tailleSubspace)
  return(rootNode)
}

# A partir d'un arbre de donnée, donne les probabilités par classes estimées pour un item 
# Input :
#     decisionTreeModel : un arbre de décision
#     item : dataframe d'une ligne
predictViaDT <- function(decisionTreeModel, item){
  node = decisionTreeModel
  
  # parcours de l'arbre de décision
  while(TRUE){
    # si le noeud n'a pas de feuilles, on retourne sa valeur V
    if(is.null(node$L) && is.null(node$R)){
      return(node$V)
    }
    
    # application de la condition du noeud :
    cond = paste("item['",node$var,"']",node$cond,sep="")
    goToRight = eval(parse(text=cond))
    
    if(goToRight){
      node = node$R
    } else {
      node = node$L
    }
  }
}

# apply the predictViaDT function over a dataset, selecting for each line the
# class with the highest class probability
applyDTonDataset <- function(decisionTreeModel,dataset){
  result = t(apply(dataset,1,function(i){predictViaDT(decisionTreeModel,i)}))
  prediction = apply(result,1,function(l){names(l[which.max(l)])})
  return(prediction)
}

#### Tools functions ####

# Calcul de l'entropie d'une source de données
classificationEntropy <- function(data,target){
  if(nrow(data) == 0) return(0)
  targetCol = t(as.vector(data[,target]))
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

# Même fonction, mais plus simple
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

# Détermine le critère de séparation d'une variable quantitative
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

# Détermination du critère de séparation d'une variable qualitative
splitFacVar <- function(data,splitVariable,target,minLeafSize=1){
  
  modalities = as.vector(t(unique(data[splitVariable])))
  
  lowestInpurity = NULL
  lowestInpurityLeft = NULL
  lowestInpurityRight = NULL
  lowestInpurityCond = NULL
  modIndex = 1
  
  # trouver la meilleure modalité
  for(i in 1:length(modalities)){
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

# calcule la probabilité de chaque classe pour la feuille actuelle
leafValue <- function(data,target,classes){
  proba = sapply(classes,function(e){mean(data[target]==e)})
  return(proba)
}

# fonction récursive, prenant en entrée un noeud et un jeu de données,
# séparant ce jeu de donnée en 2, et ajoutantlui ajoutant 2 fils.
# 
expandNode <- function(node,data,availableVars,config,tailleSubspace){
  targetColumn = data[,config$target]
  # depth control
  if(node$depth >= config$maxDepth){
    # print("Maximal Depth reached")
    return(list(V=leafValue(data,config$target,config$targetClasses)))
  }
  
  availableVarsRand <- availableVars[sample(tailleSubspace)]

  
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
  while(varIndex<length(availableVarsRand)){
    varIndex = varIndex + 1
    splitVar = availableVarsRand[varIndex]
    newSplit = list(var=splitVar)
    
    varType = class(as.vector(t(data[splitVar]))[1])
    if(varType == "numeric"){
      splitPosition = splitNumVar(data,splitVar,config$target,config$minLeafSize)
      newSplit$cond = paste(">=",splitPosition)
      newSplit$L = data[data[splitVar]<splitPosition,]
      newSplit$R = data[data[splitVar]>=splitPosition,]
    } else {
      splitMod = splitFacVar(data,splitVar,config$target,config$minLeafSize)
      # newSplit$cond = paste("==",splitMod)
      newSplit$cond = paste("=='",splitMod,"'",sep = "")
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
  if(!is.null(split)) {
    node$cond = split$cond
    node$var = split$var
    node$L = expandNode(list(depth=node$depth+1),data=split$L,availableVars,config=config,tailleSubspace)
    node$R = expandNode(list(depth=node$depth+1),data=split$R,availableVars,config=config,tailleSubspace)
    return(node)
  } else {
    
    return(list(V=leafValue(data,config$target,config$targetClasses)))
    
  }
  
  
}



