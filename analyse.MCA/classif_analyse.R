#' @description Fct qui réalise, décrit et résume une analyse des correspondances multiples
#' Elle permet un mode interactif où le choix relatif au nombre d'axes est réalisé au fur et à mesure de la présentation des axes (ask=TRUE)
#' @param donnee: dataframe, base de données à partir de laquelle souhaite faire l'ACM
#' @param nbaxe: integer, nombre d'axes à garder pour résumer l'information dans l'ACM
#' @param ask: booleen, par défaut FALSE. Si TRUE, permet à l'utilisateur d'interagir avec l'interface en choisissant
#' notamment au fur et à mesure le nombre d'axes pour l'ACM.
#' @param visu: booleen, FALSE par défaut. Si TRUE: plot descriptifs des axes (prend plus de temps en termes de traitement)
#' @return list(): renvoie une liste de quatre éléments décrivant la CAH:
#' resume.eff.classes: dataframe, résume l'effectif par classe
#' var.caracterisant.classif: dataframe: résume les variables caractérisant la CAH
#' cara.clas.axes: list(dataframe): liste de dataframe caractérisant les classes de la CAH par rapport aux dimensions de l'ACM
#' cara.clas.moda: catdes: caractérise les classes par rapport aux modalités des variables
#' @title La fonction "classif_analyse"
#' @examples classif_analyse(MCA(base_recours),nb.clas=3,ask=FALSE,visu=FALSE)
#' @author Romane Tavernier
#' @export

classif_analyse<-function(res.mca,nb.clas=NA,ask=FALSE,visu=FALSE){
  #ask pour voir si veut interaction
  #visu: booleen, FALSE par défaut. Si TRUE: affiche les graphes résumant les classes
  if(ask){
    HCPC(res.mca, graph = TRUE)->classif
  } else{
    if(!is.na(nb.clas)){
      HCPC(res.mca,nb.clust = nb.clas, graph = FALSE)-> classif
    } else {HCPC(res.mca, graph = FALSE)-> classif}
    if(visu){
      plot(classif,choice = c("bar"))
      plot(classif,choice = c("map"),draw.tree = FALSE)
    }
  }
  nb.clas<-classif$call$t$nb.clust
  t<-table(classif$data.clust$clust)
  resume.eff.classes<-as.data.frame(t)
  colnames(resume.eff.classes)=c("Num de la classe","Nb d'individus")

  data.frame(round(classif$desc.var$test.chi2,4)[classif$desc.var$test.chi2[,1]<0.05,])->var.caracterisant.classif

  #caractérisation classe par les axes
  cara.clas.axes=list()
  for (i in 1: length(classif$desc.ind$para)){
    print(paste("Caractérisation de la classe",i))
    print(round(classif$desc.axes$quanti[[i]],3))
    if(ask&(i<length(classif$desc.ind$para))){
      readline("Faire entrée pour voir la caractérisation de la classe suivante")
    }
    cara.clas.axes[[i]]<-data.frame(round(classif$desc.axes$quanti[[i]],3))
    names(cara.clas.axes)<-paste("classe",1:i)
  }

  #catdes
  catdes(donnee = classif$data.clust, num.var = which(names(classif$data.clust)=="clust"))->CATd

  return(list("resume.eff.classes"=resume.eff.classes,"var.caracterisant.classif"=var.caracterisant.classif,"cara.clas.axes"=cara.clas.axes,"cara.clas.moda"=CATd))
}
