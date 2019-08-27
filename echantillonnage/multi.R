#' @description fct qui multiplie les valeurs d'un vecteur numérique nommé avec une valeur numerique nommée
#' @param val: numeric, représente la valeur multipliée avec chaque élément du vecteur
#' @param vec: numeric vector with names
#' @param nom.val: character: name of the unique value "val"
#' @return named numeric vector: nom et valeur croisé de val et vec: "val.vec"
#' @title La fonction "multi"
#' @examples multi(val=0.5,vec=c("Ain"=0.1,"Isere"=0.4,"Rhône"=0.3,"Saone"=0.2),nom.val="TRUE")
#' @export

multi<-function(val,vec,nom.val){
  res=c()
  for(i in 1:length(vec)){
    r<-val*vec[i]
    nom<-paste(nom.val,".",labels(vec)[i],sep="")
    s=c(r)
    names(s)=nom
    res=c(res,s)
  } 
  return(res)
}