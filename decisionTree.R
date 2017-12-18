
# Stupid decision tree :
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
  
  
  a = list(cond=">=18", var="X1",
           L=list(cond=">2", var="X2",
                  L=list(V=FALSE),
                  R=list(V=TRUE)), 
           R=list(V=TRUE))
  return(a)
}

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
    print(paste("Depth : ",currentDepth,"condition =",cond," = ",goToRight))
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

# split a dataset in 2 dataset, by a criteria based on a specified split variable
# return a list(L = left dataset, R = right dataset, cond = condition, var = splitingVariable)
splitDataset <- function(data,splitVariable,classes){
  if(is.numeric(data[,splitVariable])){
    
    # for now, the split point is very na?ve, it's only the mean of the split variable
    # To improve it, we could use the weighted mean between the class centers
    splitPoint = mean(as.vector(t(data[splitVariable])))
    left = data[data[splitVariable]<splitPoint,]
    right = data[data[splitVariable]>=splitPoint,]
    cond = paste(">=",splitPoint,sep="")
    return(list(L=left,R=right,cond=cond,var=splitVariable))
  } else {
    modalities = unique(t(as.vector(data[splitVariable])))
    
    # For now, we just select a random modality, and split on it
    # later, we could take in account the target class repartition by modality
    modality = modalities[sample(1:length(modalities),1)]
    left = data[data[splitVariable]!=modality,]
    right = data[data[splitVariable]==modality,]
    cond= paste("=='",modality,"'",sep="")
    return(list(L=left,R=right,cond=cond, var=splitVariable))
  }
}

expandNode <- function(node,data,availableVars,config){
  impurity = 1
  targetColumn = data[,config$target]
  availableClasses = unique(targetColumn)
  if(length(availableClasses)==1){
    # node is pure
    return(list(V=availableClasses[1]))
  }
  
  if(node$depth<config$maxDepth){
    split = list()
    varIndex = 0
    while(varIndex>length(availableVars)){
      varIndex = varIndex + 1
      splitVar = availableVars[varIndex]
      split = splitDataset(data,splitVar,availableClasses)
      N = nrow(data)
      nL = nrow(split$L)
      nR = nrow(split$R)
      print(paste("nL = ",nL, " nR = ",nR, " N = ",N))
      
      # total impurity
      impurityL = classificationEntropy(split$L,config$target)
      impurityR = classificationEntropy(split$R,config$target)
      impurity = (nL/N) * impurityL + (nR/N) * impurityR
      print(paste("impurityLeft = ",impurityL," impurityRight = ",impurityR," totalImpurity = ",impurity))
    }
    node$cond = split$cond
    node$var = split$var
    node$L = expandNode(list(depth=node$depth+1),data=split$L,availableVars=availableVars,config=config)
    node$R = expandNode(list(depth=node$depth+1),data=split$R,availableVars=availableVars,config=config)
    return(node)
    }
}



