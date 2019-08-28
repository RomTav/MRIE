#' @description Fct qui réalise, décrit et résume une analyse des correspondances multiples et la classification qui s'en suit
#' Elle permet un mode interactif où les choix relatifs aux nombres d'axes ou de classes sont réalisés au fur et à mesure de la
#' présentation des axes (interagir=TRUE)
#' Elle permet aussi de spécifier les arguments de la fonction MCA de r
#' @param donnee: dataframe, base de données à partir de laquelle souhaite faire l'ACM + CAH
#' @param nb.axe: integer, nombre d'axes à garder pour résumer l'information dans l'ACM
#' @param nb.classe: integer, nombre de classes désirées pour la classification
#' @param interagir: booleen, par défaut FALSE. Si TRUE, permet à l'utilisateur d'interagir avec l'interface en choisissant
#' notamment au fur et à mesure le nombre d'axes pour l'ACM ou de classes pour la CAH
#' @param visual: booleen, FALSE par défaut. Si TRUE: plot descriptifs des axes et des classes (prend plus de temps en termes de traitement)
#' @return list(): renvoie une liste découpée en deux sous-listes décrivant respectivement l'ACM et la CAH.
#' Pour l'ACM: rend le résultat de la procédure MCA() de r, et un résumé de l'étude des axes
#' Pour la CAH: rend un résumé des effectifs par classes, un dataframe comprennant les variables caractérisant le plus la classification,
#' une caractérisation des classes par axes, et le résultat de catdes qui caractérise les classes par les modalités des variables
#' @title La fonction "acm_hcpc_analyse"
#' @examples acm_hcpc_analyse(base_recours,quali.sup = data.recours.sup ,nb.classe=2,nb.axe = 1,interagir = FALSE, visual = FALSE)
#' @author Romane Tavernier
#' @export

acm_hcpc_analyse<- function(donnee,nb.axe=NA,nb.classe=NA,interagir=FALSE,visual=FALSE,...){
  if(nb.axe<=1){stop("Choisir un nombre d'axe supérieur à 1 car après commande HCPC de r pour classification refuse le traitement")}
  acm<-acm_analyse(donnee,nbaxe=nb.axe,ask=interagir,visu=visual,...)
  if(interagir){readline(prompt = "appuyer sur entrée pour passer à la classification")}
  classifi<-classif_analyse(acm[[1]],nb.clas = nb.classe,ask = interagir,visu=visual)
  res<-list("description ACM"=acm,"description HCPC"=classifi)
  return(res)
}
