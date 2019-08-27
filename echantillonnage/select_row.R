#' @description function to take a Random Proportional Stratified Sample (RPSS) of size n
#' @param data_sous_groupe data.frame sur lequel échantillonner
#' @param poids : list comporte les noms des variables de contrôle
#' @param rep: booleen, si TRUE signifie que peut prendre une ligne du dataframe plusieurs fois, FALSE une unique fois
#' @param n taille de l'échantillon
#' @return data.frame: echantillon du dataframe initial
#' @references Voir le package "splitstackshape"
#' @title La fonction "select_row"
#' @examples select_row(test,poids=list(test$Professionnel,test$Rural),rep=TRUE,n=50)
#' @export


select_row <- function(data_sous_groupe,poids=list(),rep=TRUE,n=1){
  #install.packages("splitstackshape")
  library(splitstackshape)
  if (length(poids)!=0){
    data_sous_groupe$vec.interaction<-interaction(poids)
    df.poids<- data.frame(prop.table(table(data_sous_groupe$vec.interaction)))
    merge(x = data_sous_groupe, y = df.poids, by.x = "vec.interaction", by.y = "Var1", all.x = TRUE)->donnee
    if(n>nrow(data_sous_groupe)){
      if(rep==FALSE){
        stop("ATTENTION: \n Impossible de selectionner plus de reponses que celles existantes dans la base si replace=FALSE")
      }
    }
    res<-donnee[sample(nrow(donnee),size=n,replace = rep,prob=donnee$Freq),]
  }
  else {res<-donnee[sample(nrow(donnee),size=n,replace = rep),]}
  return(res)
}