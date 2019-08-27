#' @description fonction qui permet de comparer l'effectif réel ou tiré par sous groupe selon une taille d'échantillon définie
#' @param var_cri: list de variable d'un dataframe qui servent de critere pour former des sous-groupes
#' @param n_var_cri: vecteur de nom des variables critères
#' @param propor: list de vecteur numerique nommé normalisé représentant la proportion réel dans chaque sous-groupe
#' @param total: numeric, nombre d'individu à selectionner au total pour l'échantillon,  attention processus de sélection peut amener à ech plus petit
#' @return data.frame: variable caractérisant les sous-groupes avec leur effectif réel et celui de l'echantillon, ainsi que l'écart entre ces deux effectifs
#' @title La fonction "compar_eff"
#' @examples compar_eff(var_cri=list(donnee$Departement,donnee$Association),n_var_cri=c("Departement","Association"),propor=list(prop_dep,prop_asso),total=288)
#' @export

compar_eff<- function(var_cri=list(),n_var_cri=c(),propor=list(),total){  
  if(tryCatch(length(var_cri)!=length(propor))){
    stop(paste("ATTENTION: il doit y avoir autant de vecteur spécifiant la proportion des variables de critère dans 'propor' que de variables spécifiées dans var_cri. Ici la longueur de var_cri est de",length(var_cri),"alors que celle de propor est de",length(propor),"."))
    }
  eff_ech=eff(li=propor,tot = total) #calcul eff par sous-ech
  eff=as.data.frame(do.call(table,var_cri)) #creer dataframe eff et modifie non colonne pour effectif réel nommé Freq par défaut
  names(eff)=c(n_var_cri,"Effectif_Reel")
  s<-c()
  for (i in 1:nrow(eff)){
    e<-unlist(strsplit(labels(eff_ech)[i],".",fixed = TRUE))
    s<-cbind(s,c(e,eff_ech[i]))
  }
  d<-as.data.frame(t(s)) #crée dataframe d et modifie nom col pour eff échantillon
  names(d)=c(n_var_cri,"Effectif_echantillon")
  res<-merge(eff,d,by=n_var_cri)
  res$Diff=rep(NA,nrow(res))
  res$Rapport=rep(NA,nrow(res))
  for (i in 1:nrow(res)){
    res$Effectif_echantillon<-as.numeric(as.character(res$Effectif_echantillon))
    res[i,"Diff"]=-res[i,"Effectif_Reel"]+res[i,"Effectif_echantillon"]
    res[i,"Rapport"]=round(res[i,"Effectif_echantillon"]/res[i,"Effectif_Reel"],2)
  }
  return(res)
}

