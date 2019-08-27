#' @description fonction qui permet de comparer certains indicateur obtenu selon différentes valeurs de n= eff ech voulu
#' @param var_cri: list de variable d'un dataframe qui servent de critere pour former des sous-groupes
#' @param n_var_cri: vecteur de nom des variables critères
#' @param propor: list de vecteur numerique nommé normalisé représentant la proportion réel dans chaque sous-groupe
#' @param n: numeric, nombre théorique d'individu à selectionner au total pour l'échantillon, attention processus de sélection peut amener à ech plus petit
#' @return data.frame: variable caractérisant l'écart entre l'effectif théorique et observé des sous-groupes
#' @title La fonction "n_analyse"
#' @examples n_analyse(var_cri = list(donnee$Departement,donnee$Association),n_var_cri = c("Departement","Association"),propor = list(prop_dep,prop_asso),n=288)
#' @export


n_analyse<-function(var_cri=list(),n_var_cri=c(),propor=list(),n=100){
  #n: taille echantillon pour qui test + ou - 20% de l'eff total
  effectif=compar_eff(var_cri=var_cri,n_var_cri=n_var_cri,propor=propor,n)
  eff_tot=sum(effectif$Effectif_Reel)
  eff_ech=sum(effectif$Effectif_echantillon)
  n_min=round(eff_ech-0.2*eff_tot)
  n_max=round(eff_ech+0.2*eff_tot)
  res=c()
  for (i in n_min:n_max){
    effe=compar_eff(var_cri=var_cri,n_var_cri=n_var_cri,propor=propor,i)
    m=round(mean(effe$Rapport-1),2)
    m_p=round(weighted.mean(effe$Rapport-1,w=effe$Effectif_echantillon),2)
    inf=sum((effe$Rapport-1)<0)
    sup=sum((effe$Rapport-1)>0)
    eff_ech=sum(effe$Effectif_echantillon)
    nb_sup=sum(effe$Diff[effe$Diff<0])
    nb_dup=sum(effe$Diff[effe$Diff>0])
    res=cbind(res,c(i,m,m_p,inf,sup,eff_ech,nb_sup,nb_dup))
  }
  res=as.data.frame(t(res))
  colnames(res)=c("n","Moyenne","Moyenne pondérée par eff réel","Nb de ss-gp où répondant(s) supprimé(s)","Nb de ss-gp ou répondant(s) dupliqué(s)","Effectif echantillon final","Nb de répondants théoriques à supprimer","Nb de répondants théoriques à dupliquer")
  return(res)
}