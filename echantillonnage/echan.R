#' @description function to sample according to criteria and weights
#' @param data_totale data.frame sur lequel échantillonner
#' @param nom_var_cri: c("","") vecteur de caractère:  nom des variable utilisées comme critère de division en sous-echantillons
#' @param var_control: c("txt","txt2"): vecteur de caractère: nom des variables de contrôle
#' @param prop: list de vecteur numerique nommé normalisé représentant la proportion réel dans chaque sous-groupe
#' @param repet: booleen, dit si peu prendre deux fois le même individu dans l'echantillon final
#' @param n: effectif total désiré pour l'echantillon, attention processus de sélection peut amener à ech plus petit
#' @return list(data.frame): liste contenant l'echantillon du dataframe initial et un dataframe comparant l'effectif réel et celui de l'échantillon par sous-groupe
#' @title La fonction "echan"
#' @examples echan(donnee,nom_var_cri = c("Departement","Association"),var_control=c("Rural"),prop=list(prop_dep,prop_asso),repet = TRUE,n=200)
#' @export

echan <- function(data_totale,nom_var_cri=c(),var_control=c(),prop,repet=TRUE,n=100){
  eff=NULL
  n_analyse=NULL
  if(length(nom_var_cri)!=0){ #cas où a une ou des variables de critère
    var_critere=lapply(nom_var_cri, function(i){data_totale[,i]})
    for (i in 1:length(var_critere)){
      if(tryCatch(length(levels(var_critere[[i]])))!=length(prop[[i]])){
        stop("Il faut qu'il y ait autant de levels dans chacune de vos variables critères que de proportion indiquée")
      }
    }
    names(var_critere)<-nom_var_cri
    eff=compar_eff(var_cri = var_critere,n_var_cri=nom_var_cri,propor = prop,total=n)
    n_analyse=n_analyse(var_cri=var_critere,n_var_cri=nom_var_cri,propor=prop,n=n)
    if(0%in%eff$Effectif_Reel){res=NULL} else {
      merge(data_totale,eff,by= nom_var_cri)->w
      z=div(w,var_critere = w[,nom_var_cri]) #divise en sous-groupe
      res=data.frame()
      for (i in 1:length(z)){ # parcours toutes les sous-bases de données
        if(nrow(z[[i]])!=0){
          s=subset(z[[i]],select=-c(new))
          if (length(var_control)!=0){ #cas où a une ou des var de contrôle
            #afin que poids via var_control soit défini pour chaque ligne du sous-ech et pas sur la base de données totale
            li=list()
            for (j in var_control){
              li[j]=z[[i]][j]
            }
          } else{li=c()}
          h=select_row(data_sous_groupe = s,poids = li ,rep = repet,n = z[[i]]$Effectif_echantillon[1] )
          names(h)
          res=rbind(res,h)
        }
      }
    }
  } else if(length(var_control)!=0){
    li=list()
    for (j in var_control){
      li[j]=data_totale[j]
    }
    res=select_row(data_sous_groupe = data_totale,poids = var_control ,rep = repet,n = n)
  }
  else{res=select_row(data_sous_groupe = data_totale,rep=repet,n=n)}
  if(!is.null(res)){res=subset(res,select=names(data_totale))}
  return(list("echantillon"=res,"resume"=eff,"n_analyse"=n_analyse))
}
