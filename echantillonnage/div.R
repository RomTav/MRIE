#' @description function to take a split a dataframe according to modalities of some variable
#' @param data data.frame initial qui va être subdivisé
#' @param var_critere : list(var1,var1): liste des variables dont les modalitées sont utilisées pour créer des sous-groupes dans notre dataframe
#' @return liste de vecteurs contenant les valeurs du groupe, chaque groupe ayant une modalité de chacune des variables criteres
#' @references Voir le package "splitstackshape"
#' @examples div(data=cars,var_critere=list(cars$speed))
#' @export

div <-function(data,var_critere=list()){
  data$new=interaction(var_critere)
  z=split(data,data$new)
  return(z)
}

