#' @description fct qui calcule l'effectif proportionnel pour chaque sous-groupe obtenu par croisement des variables de li, afin d'atteindre un certain effectif total
#' @param li: list de vecteur numerique nommé et normalisé
#' @param tot: numeric: represente l'effectif total voulu
#' @return vecteur contenant l'effectif pour chaque sous-groupe nommé
#' @title La fonction "eff"
#' @examples eff(li = list(c("TRUE"=0.5,"FALSE"=0.5),c("Ain"=0.1,"Isere"=0.4,"Rhône"=0.3,"Saone"=0.2)),tot=100)
#' @export


eff<-function(li,tot){
  for (i in 1:length(li)){ #vérifie que somme proportion égale à 1 pour chaque variable critere
    if(tryCatch(expr = (sum(li[[i]])!=1))){stop(paste("La somme des proportions pour la variable critère",names(li)[[i]],"doit être égale à 1"))}
  }
  t=length(li)
  res=li[[1]]
  while(t>1){
    res=c()
    t=t-1
    for (i in 1:length(li[[1]])){
      r<-multi(li[[1]][i],li[[2]],labels(li[[1]])[i])
      res<-c(res,r)
    }
    li[[1]]=res
    li=li[-2]
  }
  return(round(res*tot,0))
}
