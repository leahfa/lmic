#' Make  a one-dimensional correlation table
#'
#' Correlate a single vector against several vectors; spearmans by default
#' Order of samples MUST BE SAME
#'
#'
#'
#'
#' @param datobj a matrix/dataframe of numerical data, usually taxa RAs
#' @param yfv vector of interest
#' @param dr num digits to round after 0
#' @return table of all correlations, giving R, p abd q
#'
#'
#' @export


cor.1dim<-function(datobj,yfv,dr,method="spearman",...) {

Rho=c()
p=c()
Taxon=c()
for (i in 1:ncol(datobj) ) {
  if (method=="spearman") {
    cor_res= cor.test(datobj[ ,i], yfv,method="spearman",...)
  }
  if (method=="pearson") {
    cor_res= cor.test(datobj[ ,i], yfv,method="pearson",...)
  }

  Rho[i]=round(cor_res$estimate,3)
  p[i]=round(cor_res$p.value,dr)
  Taxon[i]=colnames(datobj)[i]




}
sum_cor=cbind.data.frame(Taxon,Rho,p)
sum_cor$FDR=round(p.adjust(sum_cor$p, "fdr"),dr)
sum_cor=sum_cor[order(sum_cor$FDR),]
return(sum_cor)
}

