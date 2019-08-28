#' @description Fonction qui calcule le nombre d'axes à conserver lors d'une ACM selon le critère du scree-test
#' @param res.mca: résultat de la fonction MCA() implémentée sous R
#' @param nmax: integer, nombre maximum d'axes à garder
#' @return integer, correspond au nombre d'axes à conserver pour résumé l'information lors d'une ACM
#' @title La fonction "scree_test"
#' @examples scree_test(MCA(base_recours),nmax=15)
#' @author Romane Tavernier
#' @export


scree_test<-function(res.mca,nmax=15){
  #calcul critère du scree test pour choix nb axes acm
  #res.mca.eig correspond au resultat de l'ACM
  res.mca$eig[1:nmax,2]-res.mca$eig[2:(nmax+1),2]->diff1
  diff1[1:(length(diff1)-1)]-diff1[2:length(diff1)]-> diff2
  nb<-(which(diff2<0)[1])+1
  names(nb)<-"nb axes ACM"
  return(nb)
}
