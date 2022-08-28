#' Feature filtration by prevalence and min abundance
#' 
#' FIlter taxa based on prevalence and on an abundance threshold
#'
#'
#'
#' 
#' @param datobj matrix of numerical data
#' @param x prevalence threshold;features whocer pervalence is equal or smaller tahn x 
#' will be dropped
#' @param tmax minimal RA/abundance threshold; features whose maximal RA/abundance is lower 
#' than this are dropped (i.e., must pass this threshold in at least ONE sample to be 
#' retained). Note this step is run AFTER the prevalence filtration. 
#' @return filtered matrix



filtColsTmax<-function(datobj,x,tmax ){
  datobj.b=ifelse(datobj>0,1,0)
  keep=which(colSums(datobj.b)>=x)
  datobj.s<-datobj[ ,keep, drop=FALSE]
  max.ra<-apply(datobj.s,2,max)
  keep2<-which(max.ra>tmax)
  datobj.f<-datobj.s[ , keep2, drop=FALSE]
 
  return(datobj.f)
}
