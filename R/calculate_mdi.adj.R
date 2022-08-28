#' Calculate Microbial Dysbiosis Index (MDI, Gevers 2014)
#'
#' Calculate Microbial Dysbiosis Index (log ratio of sum RAs of
#' taxa incraesed in IBD/sum RAs of taxa decraesed in IBD); from Gevers 2014
#' Bit note R. gnavus moved to "bad side" (nominator)
#'
#'
#'
#' @param datobj a matrix of taxa relative abundances; sequencing sample identifiers are in row.names.
#' @param depth rarefaction depth (uniform sample depth)
#'
#'
#' @return a numerical vector of MDI values
#' @export









calculate.mdi.adj<-function(dat,depth) {


b=c("Enterobacteriaceae", "Pasteurellaceae", "Neisseriaceae", "Gemellaceae", "Fusobacteriaceae","Veillonellaceae")
g=c("Erysiopelotrichaceae","Bifidobacteriaceae","Bacteriodales","Clostridiales")
mdi=c()
contras=c("gnavus","torques")
for ( i in 1:nrow(dat)) {
  total.b=0
  total.g=0
  total.contras=0
  for (j in 1:length(b)) {
   x=sum(dat[i,grep(b[j],colnames(dat))])
   total.b=total.b+x
  }

  for (j in 1:length(g)) {
    x=sum(dat[i,grep(g[j],colnames(dat))])
    total.g=total.g+x
  }
  for (j in 1:length(contras)) {
    x=sum(dat[i,grep(contras[j],colnames(dat))])
    total.contras=total.contras+x
  }

  total.b<-total.b+total.contras
  total.g<-total.g-total.contras
  if (total.b==0) {total.b=3/depth}
  if (total.g==0) {total.g=3/depth}
  ####figure i the contras: ####

 mdi[i]=log(total.b/total.g)
# print(paste("Round", i, ",Sample",rownames(dat)[i]," , sum covered:",total.b+total.g))
}
names(mdi)=rownames(dat)
return(mdi)
}
