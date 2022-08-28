#' Make a 2-dimensional correlation table
#'
#' Correlate several vector against several vectors, spearmans by default
#' ORDER OF SAMPLES MUST BE SAME IN `x` and `y``
#'
#'
#'
#'
#' @param x.cors a matrix/data frame  of numerical vectors
#' @param y.cors a matrix/data frame  of numerical vectors; sample order same as x.cors
#' @param dig num digits to round after 0
#' @param cut.level if set to "yes", will strip taxa names to genus level
#' @return table of all correlations, giving R, p and q
#'
#'
#' @export


cor.2dim<-function(x.cors,y.cors,dig,cut.level="no") {
  cors=list()
  print(dig)
  for (i in 1:ncol(x.cors)) {
    p=c()
    r=c()
    for (j in 1:(ncol(y.cors))){
      p[j]=signif(cor.test(x.cors[ ,i],y.cors[ ,j],method="spearman")$p.value,3)
      r[j]=signif(cor.test(x.cors[ ,i],y.cors[ ,j],method="spearman")$estimate,3)
    }
    temp=as.data.frame(cbind.data.frame(colnames(y.cors),rep(colnames(x.cors)[i],ncol(y.cors)),r,p,stringsAsFactors=F))
    temp$q<-round(p.adjust(temp$p,"fdr"),dig)
    if (cut.level=="yes"){temp$tax.short<- strip.names.silva.dada(temp[ ,1],5) }
     cors[[i]]<-temp
     }
  res=as.data.frame(do.call("rbind",cors))
  colnames(res)=c("Var1","Var2","Rho","p","q")
  res=res[order(res$q),]

  #write.csv(res,paste0(path,"log10marker_interactions.csv"),row.names=F)
  return(res)
}


cor.2dim_notSorted<-function(x.cors,y.cors) {
  cors=list()
  for (i in 1:ncol(x.cors)) {
    p=c()
    r=c()
    for (j in 1:(ncol(y.cors))){
      p[j]=signif(cor.test(x.cors[ ,i],y.cors[ ,j],method="spearman")$p.value,3)
      r[j]=signif(cor.test(x.cors[ ,i],y.cors[ ,j],method="spearman")$estimate,3)
    }
    temp=as.data.frame(cbind.data.frame(colnames(y.cors),rep(colnames(x.cors)[i],ncol(y.cors)),r,p,stringsAsFactors=F))
    temp$q<-round(p.adjust(temp$p,"fdr"),dig)
    cors[[i]]<-temp
  }
  res=as.data.frame(do.call("rbind",cors))
  res$q2=round(p.adjust(res$p,"fdr"),dig)
  colnames(res)=c("Var1","Var2","Rho","p","q","q2")
 # res=res[order(res$q),]
  #write.csv(res,paste0(path,"log10marker_interactions.csv"),row.names=F)
  return(res)
}
