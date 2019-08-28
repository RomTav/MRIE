#' @description Fct qui calcule l'inertie moyenne à partir du résultat d'une ACM et le nombre de dimensions à conserver pour résumer l'information.
#' Les dimensions gardées sont celles dont l'inertie est supérieure en valeur à l'inertie moyenne
#' @param res.mca: résultat de la fonction MCA() implémentée sous R
#' @return list(): renvoie une liste de deux éléments décrivant l'ACM: l'inertie moyenne et le nombres de dimension à garder selon ce critère
#' @title La fonction "inertie_moy"
#' @examples inertie_moy(MCA(base_recours))
#' @author Romane Tavernier
#' @export


inertie_moy<-function(res.mca){
  inertie<-round((1/nrow(res.mca$eig)),5)
  axe<-(which(res.mca$eig[,2]<=inertie*100)[1]-1)
  res<-c(round(inertie,3),axe)
  names(res)=c("Inertie moyenne","Nb d'axes selon inertie moyenne")
  return(res)
}
