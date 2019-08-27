#' @description calcul distance entre ligne du dataset aux centres
#' @param dataset data.frame à partir duquel calcule distance aux centres
#' @param centre: dataframe: chaque ligne correspond à un centre d'une classification
#' @param ncp: nombre d'axes à garder pour l'ACP normalisant et projetant les observation des dataframes avant le calcul des distances,
#' si ncp non spécifier: prend pour ncp le nombre d'axes pour lequel le pourcentage d'information résumé est au plus proche de 80%
#' @return dataframe avec pour chaque observations du dataset: les variables de départ, les distances aux n centres et celui dont elle est le plus proche
#' @references Voir le package "FactoMineR", "factoextra" et "proxy"
#' @title La fonction "dist.centre"
#' @examples dist.centre(data2012,data2016,ncp=7,method="correlation")->test
#' @author Romane Tavernier
#' @export

dist.centre <- function(dataset,centre,ncp=0,...){
  #install.packages
  library(factoextra)
  library(FactoMineR)
  library(proxy)
  if(length(which(!names(dataset)%in%names(centre)))+length(which(!names(centre)%in%names(dataset)))!=0){stop("ERROR:: !!! Les noms des variables caractérisant le dataset et le centre doivent être similaires !! \n \n")}
  rbind(centre,dataset)->donnee
  n=nrow(centre)
  if (ncp==0){
    FactoMineR::PCA(X = donnee,ind.sup = 1:n, graph = FALSE, scale.unit = TRUE)->ACP.pam
    which.min(abs(ACP.pam$eig[,3]-80))->ncp
  }
  FactoMineR::PCA(X = donnee,ind.sup = 1:n, ncp = ncp, graph = FALSE, scale.unit = TRUE)->ACP.pam
  proxy::dist(x = ACP.pam$ind$coord,y=ACP.pam$ind.sup$coord,...)->dist.ACP.pam
  as.matrix(dist.ACP.pam)->mat.dist
  as.data.frame(mat.dist[,1:n])->res
  res$CODGEO<-rownames(res)
  colnames(res)=c(paste("Dist obs au gp",1:n,"de la classif"),"CODGEO")
  res$Dist.min.gr<-NA
  for (i in 1:nrow(res)){
    res$Dist.min.gr[i]<-which.min(res[i,1:n])
  }
  res<-cbind(dataset,res)
  return(res)
}
