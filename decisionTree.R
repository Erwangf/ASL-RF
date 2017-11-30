
# Stupid decision tree :
# exemple de retour : TRUE if 
# var "1" >= 18 
# or 
# var"1" <18 et var2>2
# pour l'instant, paramètre inutilisés
createDecisionTreeModel <- function(data,vars,impurity,maxDepth){
  
  
  a = list(var=1, cond=">=18", 
           L=list(var=2,cond=">2",
                  L=list(V=TRUE),R=list(V=TRUE)), 
           R=list(V=TRUE))
  return(a)
}

predictFromDecisionTree <-function(decisionTreeModel, item){
  node = decisionTreeModel
  while(TRUE){
    if(is.null(node$L) && is.null(node$R)){
      return(node$V)
    }
    
    # application de la condition du noeud :
    cond = paste("item[var]",node$cond)
    var = node$var
    goToRight = eval(parse(text=cond))
    print(paste("condition = ",cond," result => ",goToRight))
    if(goToRight){
      node = node$R
    } else {
      node = node$L
    }
  }
}

model = createDecisionTreeModel(data)

predictFromDecisionTree(model,c(17,2))
