#' @description Fct qui représente graphiquement un axe d'une ACM en présentant les variables qui contribuent majoritairement à sa construction
#' @param res.mca: résultat de la fonction MCA() implémentée sous R
#' @param num.axe: integer, numéro de la dimension à représenter
#' @param nb.var.contrib: integer, nombre de variables les plus contribuantes à la construction de l'axe à représenter
#' @return list(): renvoie une liste de deux éléments décrivant l'ACM: l'inertie moyenne et le nombres de dimension à garder selon ce critère
#' @title La fonction "visu_axe"
#' @examples visu_axe(MCA(base_recours),num.axe=1,nb.var.contrib=10)
#' @author Romane Tavernier
#' @export

visu_axe<-function(res.mca,num.axe,nb.var.contrib=10){
  dimdesc(res.mca, axes=num.axe)[[1]]$quali-> dim1
  dim1<-as.data.frame(round(dim1[,2],3))
  colnames(dim1)=c("p-value")

  fviz_contrib(res.mca, choice = "var", axes = num.axe,top = 30,sort.val = "desc")->visu1
  print(visu1)

  fviz_mca_var (res.mca, axes=c(num.axe,num.axe), col.var = "contrib",
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                repel = TRUE,
                select.var = list(contrib = nb.var.contrib))->visu2
  print(visu2)
}
