#' @description Fct qui calcul la distance des lignes d'un ou plusieurs dataframe au différents centres
#' et donne le rowname du centre qui est le plus proche pour chaque observation
#' grâce aux ...: permet de modifier les arguments du calcul de la distance: méthode par e.g. ( dist(,...) )
#' @param centre: dataframe où chaque ligne correspond aux coordonnées des centres
#' @param  datas: liste de dataframe
#' @param ncp: nombre d'axes à garder pour l'ACP normalisant et projetant les observation des dataframes avant le calcul des distances
#' si ncp non spécifier: prend pour ncp le nombre d'axes pour lequel est au plus proche de 80% de l'information résumé
#' @return list de dataframe avec colonnes utilisées pour calcul des distances,
#' une colonne avec la distance au centre pour chaque centre spécifiés et une colonne avec le nom du centre le plus proche
#' @title La fonction "dist.classif"
#' @examples load("P://Documents_de_Romane/classif_communes_INSEE/RData/data_export.RData")
#' load("P://Documents_de_Romane/classif_communes_INSEE/RData/data_acp_pam.RData")
#' data2016<-data.acp.pam
#' load("P://Documents_de_Elie/Bases_de_donnees/Rcensement de la population/Bases_RP_2015/DATAPAM.RData")
#' data2015<-data.acp.pam
#' data2015<-subset(data2015,select=-c(CLUSTER.PAM.acp))
#' rownames(data2015)<-data2015$CODGEO
#' rownames(data2016)<-data2016$CODGEO
#' l<-list(data2015[8:nrow(data2015),],data2016[8:nrow(data2016),])
#' centre<-data2015[1:7,]
#' variable<- c("EPOP","TFMP","TFp3.2","Tm20", "Tp65","TVOI","TEN5", "ICE3", "TFFNimp" ,"MEDNi" ,"TLGV", "THLM","TSDB","TINA","TCHOM", "TCDD" ,"TNSSD")
#' dist.classif(centre,l,variable,ncp = 7)->test
#' @author Romane Tavernier
#' @export


dist.classif <- function(centre,datas,variable,ncp,...){
  center<-centre[,variable]
  resultat<-list()
  for (i in 1:length(datas)){
    donne<-datas[[i]][,variable]
    d<-dist.centre(dataset = donne,centre = center,ncp = 7)
    resultat[[i]]<-(d)
  }
  return(resultat)
}
