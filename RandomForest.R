## File : Random Forest
# Author ArctoScience
# Date : 18/12 
source("decisionTree.R")

# Description #### 
# Cette fonction permet de r�aliser sur un jeu de donn�es une phase d'apprentissage d'une for�t al�atoire, c'est � dire un ensemble d'arbres de d�cision construits sur des �chantillons Bootstrap, conformement � la m�thode propos� par Breiman en 2001 
#### Input ####  
# Data : Un jeu de donn�e, en format Data Frame
# target : Le nom de la variable cible (categorisation)
#### Param�tres #### 
# numBootstrap : le nombre d'�chantillons Bootstrap 
# tailleSubspace : la taille du sous espace al�atoire (le nombre de variables que l'on conserve, cf article de Breiman) 
# impurityMethod : la mesure de l'impuret�, pour l'instant seulement "entropy"
# maxDepth : la profondeur maximale de l'arbre
# minLeafSize : le nombre minimum d'individus pour poursuivre la d�coupe
# impurityThreshold : un seuil pour la mesure d'impuret� (si impurt� < seuil, on arrete la d�coupe)
#### Output ####  
# Une list d'objet de type arbre, cf la fonction createDecisionTreeModel
RandomForest <- function(data,target,numBootstrap = 100, tailleSubspace = floor(sqrt(ncol(data) - 1)), impurityMethod="entropy", maxDepth = 2, minLeafSize = 1, impurityThreshold = 0.2){
  
  # Package n�cessaire pour la fonciton list.append
  require(rlist)
  
  #On initialise la liste de sortie
  trees <- list()
  
  # on r�alise numBootstrap fois l'op�ration de Bootstrap +cr�ation d'arbre
  for (i in 1:numBootstrap) {
    
    #Cr�ation de l'�chantillon Bootstrap
    sampleIDs <- sample(nrow(data),replace = TRUE)
    bootstrap <- data
    bootstrap[1,] <- data[sampleIDs[1],]
    for (i in sampleIDs[2:length(sampleIDs)]) {
      bootstrap[i,] <- data[sampleIDs[i],]
    }
    
    #Cr�ation de l'arbre de d�cision.
    tempTree = arbreGeneration(bootstrap,target,maxDepth,minLeafSize,impurityThreshold,tailleSubspace = tailleSubspace)
    
    # On ajoute l'arbre � la liste de sortie
    trees <- list.append(trees,tempTree)
  }
  
  #On renvoie la liste obtenue
  return(trees)
}

# Description #### 
# Cette fonction permet de r�aliser sur un jeu de donn�es une phase d'apprentissage d'une for�t al�atoire, c'est � dire un ensemble d'arbres de d�cision construits sur des �chantillons Bootstrap, conformement � la m�thode propos� par Breiman en 2001. A la diff�rence de la fonciton pr�c�dente, la phase d'apprentissage est parall�lis�e 
#### Input ####  
# Data : Un jeu de donn�e, en format Data Frame
# target : Le nom de la variable cible (categorisation)
#### Param�tres #### 
# numCore : le nombre de coeurs pour le calcul parall�le
# numBootstrap : le nombre d'�chantillons Bootstrap 
# tailleSubspace : la taille du sous espace al�atoire (le nombre de variables que l'on conserve, cf article de Breiman) 
# impurityMethod : la mesure de l'impuret�, pour l'instant seulement "entropy"
# maxDepth : la profondeur maximale de l'arbre
# minLeafSize : le nombre minimum d'individus pour poursuivre la d�coupe
# impurityThreshold : un seuil pour la mesure d'impuret� (si impurt� < seuil, on arrete la d�coupe)
#### Output ####  
# Une list d'objet de type arbre, cf la fonction createDecisionTreeModel
parallelRandomForest <- function(numCore = detectCores(), data,target,numBootstrap = 100, tailleSubspace = floor(sqrt(ncol(data) - 1)), impurityMethod="entropy", maxDepth=2, minLeafSize = 1, impurityThreshold = 0.2){
  
  # On cr�e le cluster
  require(parallel)
  cl <- makeCluster(numCore)
  
  # On exporte dans chaque noeud fils les donn�es n�cessaire � la foncitonn de cr�ation de l'arbre de d�cision
  clusterExport(cl, list("data","target","tailleSubspace","impurityMethod", "maxDepth","minLeafSize","impurityThreshold"),envir = environment())
  
  # On source le code de l'algorithme de cr�ation de l'arbre de d�cision
  clusterEvalQ(cl,source("decisionTree.R"))
  
  #Lancement en parall�le des cr�ation d'arbre
  res <- parLapply(cl, 1:numBootstrap, fun = function(i) {
    sampleIDs <- sample(nrow(data),replace = TRUE)
    bootstrap <- data
    bootstrap[1,] <- data[sampleIDs[1],]
    for (i in sampleIDs[2:length(sampleIDs)]) {
      bootstrap[i,] <- data[sampleIDs[i],]
    }
    tempTree = arbreGeneration(bootstrap,target,maxDepth,minLeafSize,impurityThreshold,tailleSubspace = tailleSubspace)
    return(tempTree)
  })
  
  # parLapply renvoie une liste, on renvoie donc res
  stopCluster(cl)
  return(res)
}

# Description #### 
# Cette fonction permet d' effectuer une classification d'un input grace � une for�t al�atoire fournie en parametre
#### Input ####  
# forest : une foret al�atoire renvoy�e par la fonciton pr�c�dente
# input : un individu 
#### Output ####  
# une liste des proportions de votes pour chaque classes (pour une cat�gorisation simple, on conserve le vote majoritaire)
predictFromForest <- function(forest,input){
  
  # Pour chaque arbre de la For�t al�atoire, on va executer la fonction predictViaDT
  predictions <-  Reduce("c",lapply(forest,function(t) {
    l <- predictViaDT(t, input)
    return(names(l[which.max(l)])[1])
  }))
  
  # on r�cup�re un ensemble de vote qu'on normalise pour obtenir une proportion
  repartitionVotes <- table(predictions) / sum(table(predictions))
  return(repartitionVotes)
}

# Description #### 
# Cette fonction permet d' effectuer une classification d'un dataframe grace � une for�t al�atoire fournie en parametre
#### Input ####  
# forest : une foret al�atoire renvoy�e par la fonciton pr�c�dente
# input : un data frame d'individu � categoriser
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