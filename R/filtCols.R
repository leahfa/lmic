#' Filter featres by prevalence
#' 
#' FIlter taxa based on prevalence only 
#' (sampels with prevalence higher than `x` are retained )
#'
#'
#'
#' 
#' @param datobj matrix of numerical data
#' @param x prevalence threshold;features whose pervalence is equal or smaller than x 
#' will be dropped
#' 
#' @return filtered matrix






filtCols<-function(dat,x ){
  dat.b=ifelse(dat>0,1,0)
  keep=which(colSums(dat.b)>x)
  dat.f=dat[,keep, drop=FALSE]
 
  return(dat.f)
}