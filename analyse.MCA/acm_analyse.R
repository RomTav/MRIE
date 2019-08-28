#' @description Fct qui réalise, décrit et résume une analyse des correspondances multiples
#' Elle permet un mode interactif où le choix relatif au nombre d'axes est réalisé au fur et à mesure de la présentation des axes (ask=TRUE)
#' Elle permet aussi de spécifier les arguments de la fonction MCA de r
#' @param donnee: dataframe, base de données à partir de laquelle souhaite faire l'ACM
#' @param nbaxe: integer, nombre d'axes à garder pour résumer l'information dans l'ACM
#' @param ask: booleen, par défaut FALSE. Si TRUE, permet à l'utilisateur d'interagir avec l'interface en choisissant
#' notamment au fur et à mesure le nombre d'axes pour l'ACM.
#' @param visu: booleen, FALSE par défaut. Si TRUE: plot descriptifs des axes (prend plus de temps en termes de traitement)
#' @return list(res.mca, dataframe): renvoie une liste décrivant l'ACM:
#' Pour res.mca: rend le résultat de la procédure MCA() de r
#' Pour le dataframe: résumé étude des axes: inertie moyenne, scree-test, contribution moyenne, etc
#' @title La fonction "acm_analyse"
#' @examples acm_analyse(base_recours,quali.sup =data.recours.sup,nbaxe=2,ask=FALSE, visu=FALSE)
#' @author Romane Tavernier
#' @export

acm_analyse<-function(donnee,nbaxe=NA,ask=FALSE,visu=FALSE,...){
  #ask: pour avoir interaction ou non
  res.mca <- MCA(donnee,graph = FALSE,...)
  if(visu){  barplot(res.mca$eig[1:20,2],main="Pourcentage d'inertie par axes")}
  scree_test(res.mca)->scree.test
  inertie_moy(res.mca)->inertie.moy
  contrib_moy<-round(mean(res.mca$var$contrib),2)
  print(paste("Selon scree test, privilégie", scree.test,"axes"))
  print(paste("Selon critère d'inertie moyenne, privilégie", inertie.moy[2], "axes"))

  if(is.na(nbaxe)){
    if(ask){
      nbaxe<- tryCatch({
        test<-readline(prompt = "Combien d'axes souhaitez-vous garder pour résumer les données : \n !!! un nombre réel sera arrondi à l'entier le plus proche !!!")
        r<-round(as.integer(test))},
        warning=function(x){
          print("Erreur de type, spécifier un nombre entier ou nb d'axes pris par défaut.")
          r <- readline(prompt = "Combien d'axes souhaitez-vous: ")
          return(r)})
    }else{nbaxe<-"non renseigné"}
  }

  nbaxes <- tryCatch(nbaxes<-round(as.numeric(nbaxe)),warning=function(x){
    nbaxes=scree.test
    print(paste("Nombre d'axes choisi par défaut est",nbaxes,": choisi selon critère du scree-test. "))
    return(nbaxes)
  })
  names(nbaxes)<-"Nb d'axes choisis"
  names(scree.test)<-"Nb d'axes selon scree-test"
  resume_axe<-data.frame(inertie.moy[1],contrib_moy,format(scree.test,digits = 0),format(inertie.moy[2],digits = 0),format(nbaxes,digits =0))
  rownames(resume_axe)<-"Résumé choix axes ACM"
  colnames(resume_axe)<-c("Inertie moyenne","Contribution moyenne",names(scree.test),names(inertie.moy)[2],names(nbaxes))
  res.mca <- MCA(donnee,ncp = nbaxes,graph = FALSE,...)
  if(visu){
    for(i in 1:nbaxes){
      visu_axe(res.mca,num.axe = i)
      if(ask&(i<nbaxes)){
        attente<-readline("Cliquer sur entrée pour voir la description de la dimension suivante")
      }
    }
  }
  return(list("resultat acm"=res.mca,"résumé étude des axes"=resume_axe))
}
