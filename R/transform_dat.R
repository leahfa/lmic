
#' Transform Data
#' 
#' 
#' Apply a transformation to data; zero-impute first if necessary

#' @param trans.dat numerical data which needs to be transformed; may be matrix, 
#' dataframe or vector
#' @param impute.method methos for zero imputation - by default, half teh minimal
#'  value for that feature
#' @param transform.method method for transformation (log, log2,log10 or arcsine)
#' @return transformed data
#' @export
td<-function(trans.dat,impute.method="half-min",transform.method="log"){
  if (is.null(dim(trans.dat))) {
    y=trans.dat
    y1<-y
    if (impute.method=="half-min") { y1[which(y1==0)]<-min(y[which(y!=0)])/2}
    if (impute.method=="all.ones") {y1<-y+1}
    if (transform.method=="log10") {y2<-log10(y1)}
    if (transform.method=="log") {y2<-log(y1)}
    if (transform.method=="log2") {y2<-log2(y1)}
    if (transform.method=="arcsine") {y2<-asin(sqrt(y1))  }
    
    
  } else{
   y2<-trans.dat
  for ( i in 1:ncol(trans.dat)){
    
    y<-trans.dat[ ,i]
    
    if (impute.method=="half-min") { y[which(y==0)]<-min(y[which(y!=0)])/2}
    if (impute.method=="all.ones") {y<-y+1}
    if (transform.method=="log10") {yt<-log10(y)}
    if (transform.method=="log") {yt<-log(y)}
    if (transform.method=="log2") {yt<-log2(y)}
    if (transform.method=="arcsine") {yt<-asin(sqrt(y)) }
    y2[ ,i]<-yt
    }
  }
 
  return(y2)
}

