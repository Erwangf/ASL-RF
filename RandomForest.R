## File : Random Forest
# Author ArctoScience
# Date : 18/12 
source("decisionTree.R")

# Description #### 
# Cette fonction permet de réaliser sur un jeu de données une phase d'apprentissage d'une forêt aléatoire, c'est à dire un ensemble d'arbres de décision construits sur des échantillons Bootstrap, conformement à la méthode proposé par Breiman en 2001 
#### Input ####  
# Data : Un jeu de donnée, en format Data Frame
# target : Le nom de la variable cible (categorisation)
#### Paramètres #### 
# numBootstrap : le nombre d'échantillons Bootstrap 
# tailleSubspace : la taille du sous espace aléatoire (le nombre de variables que l'on conserve, cf article de Breiman) 
# impurityMethod : la mesure de l'impureté, pour l'instant seulement "entropy"
# maxDepth : la profondeur maximale de l'arbre
# minLeafSize : le nombre minimum d'individus pour poursuivre la découpe
# impurityThreshold : un seuil pour la mesure d'impureté (si impurté < seuil, on arrete la découpe)
#### Output ####  
# Une list d'objet de type arbre, cf la fonction createDecisionTreeModel
RandomForest <- function(data,target,numBootstrap = 100, tailleSubspace = floor(sqrt(ncol(data) - 1)), impurityMethod="entropy", maxDepth = 2, minLeafSize = 1, impurityThreshold = 0.2){
  
  # Package nécessaire pour la fonciton list.append
  require(rlist)
  
  #On initialise la liste de sortie
  trees <- list()
  
  # on réalise numBootstrap fois l'opération de Bootstrap +création d'arbre
  for (i in 1:numBootstrap) {
    
    #Création de l'échantillon Bootstrap
    sampleIDs <- sample(nrow(data),replace = TRUE)
    bootstrap <- data
    bootstrap[1,] <- data[sampleIDs[1],]
    for (i in sampleIDs[2:length(sampleIDs)]) {
      bootstrap[i,] <- data[sampleIDs[i],]
    }
    
    #Création de l'arbre de décision.
    tempTree = createDecisionTreeModel(bootstrap,target,impurityThreshold,maxDepth,minLeafSize,tailleSubspace = tailleSubspace)
    
    # On ajoute l'arbre à la liste de sortie
    trees <- list.append(trees,tempTree)
  }
  
  #On renvoie la liste obtenue
  return(trees)
}

# Description #### 
# Cette fonction permet de réaliser sur un jeu de données une phase d'apprentissage d'une forêt aléatoire, c'est à dire un ensemble d'arbres de décision construits sur des échantillons Bootstrap, conformement à la méthode proposé par Breiman en 2001. A la différence de la fonciton précédente, la phase d'apprentissage est parallèlisée 
#### Input ####  
# Data : Un jeu de donnée, en format Data Frame
# target : Le nom de la variable cible (categorisation)
#### Paramètres #### 
# numCore : le nombre de coeurs pour le calcul parallèle
# numBootstrap : le nombre d'échantillons Bootstrap 
# tailleSubspace : la taille du sous espace aléatoire (le nombre de variables que l'on conserve, cf article de Breiman) 
# impurityMethod : la mesure de l'impureté, pour l'instant seulement "entropy"
# maxDepth : la profondeur maximale de l'arbre
# minLeafSize : le nombre minimum d'individus pour poursuivre la découpe
# impurityThreshold : un seuil pour la mesure d'impureté (si impurté < seuil, on arrete la découpe)
#### Output ####  
# Une list d'objet de type arbre, cf la fonction createDecisionTreeModel
parallelRandomForest <- function(numCore = detectCores(), data,target,numBootstrap = 100, tailleSubspace = floor(sqrt(ncol(data) - 1)), impurityMethod="entropy", maxDepth=2, minLeafSize = 1, impurityThreshold = 0.2){
  
  # On crée le cluster
  require(parallel)
  cl <- makeCluster(numCore)
  
  # On exporte dans chaque noeud fils les données nécessaire à la foncitonn de création de l'arbre de décision
  clusterExport(cl, list("data","target","tailleSubspace","impurityMethod", "maxDepth","minLeafSize","impurityThreshold"),envir = environment())
  
  # On source le code de l'algorithme de création de l'arbre de décision
  clusterEvalQ(cl,source("decisionTree.R"))
  
  #Lancement en parallèle des création d'arbre
  res <- parLapply(cl, 1:numBootstrap, fun = function(i) {
    sampleIDs <- sample(nrow(data),replace = TRUE)
    bootstrap <- data
    bootstrap[1,] <- data[sampleIDs[1],]
    for (i in sampleIDs[2:length(sampleIDs)]) {
      bootstrap[i,] <- data[sampleIDs[i],]
    }
    tempTree = createDecisionTreeModel(bootstrap,target,impurityThreshold,maxDepth,minLeafSize,tailleSubspace = tailleSubspace)
    return(tempTree)
  })
  
  # parLapply renvoie une liste, on renvoie donc res
  stopCluster(cl)
  return(res)
}

# Description #### 
# Cette fonction permet d' effectuer une classification d'un input grace à une forêt aléatoire fournie en parametre
#### Input ####  
# forest : une foret aléatoire renvoyée par la fonciton précédente
# input : un individu 
#### Output ####  
# une liste des proportions de votes pour chaque classes (pour une catégorisation simple, on conserve le vote majoritaire)
predictFromForest <- function(forest,input){
  
  # Pour chaque arbre de la Forêt aléatoire, on va executer la fonciton predictFromDecisionTree
  predictions <-  Reduce("c",lapply(forest,function(t) {
    l <- predictFromDecisionTree(t, input)
    return(names(l[which.max(l)])[1])
  }))
  
  # on récupère un ensemble de vote qu'on normalise pour obtenir une proportion
  repartitionVotes <- table(predictions) / sum(table(predictions))
  return(repartitionVotes)
}

# Description #### 
# Cette fonction permet d' effectuer une classification d'un dataframe grace à une forêt aléatoire fournie en parametre
#### Input ####  
# forest : une foret aléatoire renvoyée par la fonciton précédente
# input : un data frame d'individu à categoriser
#### Output ####  
# une liste declasse (la classe majoritaire pour chaque individu)
vectorizedPredictFromForest <- function(forest,input){
  
  # On initialise le vecteur de classe predites
  predictionsbyx <- rep("none",nrow(input))
  
  # On exectue la fonciton predictFromForest pour chaque individu
  for (i in 1:nrow(input)) {
    bob <- predictFromForest(forest,input[i,])
    predictionsbyx[i] <- names(bob)[which.max(bob)][1]
  }
  
  #on renvoie le vecteur des classes
  predictionsbyx
}