
# Stupid decision tree :
# exemple de retour : TRUE if 
# var "1" >= 18 
# or 
# var"1" <18 et var2>2
# pour l'instant, paramètre inutilisés
createStupidTreeModel <- function(data,target,impurity,maxDepth){
  
  
  a = list(var=1, cond=">=18", 
           L=list(var=2,cond=">2",
                  L=list(V=FALSE),
                  R=list(V=TRUE)), 
           R=list(V=TRUE))
  return(a)
}

createDecisionTreeModel <- function(data,target,impurity,maxDepth){
    
}

# Entropy of q classes
# data : matrix q * n
classificationEntropy <- function(data,target){
  targetCol = data[,target]
  n = length(data)
  P = table(targetCol)
  nodeEntropy = 0
  for (l in 1:length(P)) {
    if(P[l]!=0){
      nodeEntropy = nodeEntropy - (P[l]/n)*log2(P[l]/n)
    }
  }
  
  return(as.vector(nodeEntropy))
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
    cond = paste("item[var]",node$cond)
    var = node$var
    goToRight = eval(parse(text=cond))
    print(paste("Depth : ",currentDepth,"condition =",var,node$cond," = ",goToRight))
    if(goToRight){
      node = node$R
    } else {
      node = node$L
    }
  }
}

#### Tests ####

# stupid tree model + predictFromDecisionTree
model = createStupidTreeModel(data)
predictFromDecisionTree(model,c(17,2))
predictFromDecisionTree(model,c(17,3))
predictFromDecisionTree(model,c(18,3))

# classification entropy
classificationEntropy(matrix(c(1,1,0,0),nrow=4),target=1) # ==> 1
classificationEntropy(matrix(c(1,1,1,1),nrow=4),target=1) # ==> 0
classificationEntropy(matrix(c(1,0,0,0),nrow=4),target=1) # ==> ~ 0.8113
classificationEntropy(matrix(c(1,1,1,0),nrow=4),target=1) # ==> ~ 0.8113

