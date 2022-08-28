#' Calculate Anaerobic Index (AI)
#'
#' Calculate an anaerobic index (log ratio of sum RAs of
#' facultataive anaerobes/sum RAd of obligate anaeroebes); analogous to MDI 
#'
#'
#'
#' @param datobj a matrix of taxa relative abundances; sequencing sample identifiers are in row.names. 
#' @param depth rarefaction depth (uniform sample depth)
#' 
#'
#' @return a numerical vector of AI values
#' @export








calculate.adi.adj<-function(dat,depth) {
  

  g=c("Clostridiales","Bifidobacteriales","Coriobacteriales","Bacteroidales","Turicibacter", "Erysipelatoclostridium",
      "Solobacterium","Selenomonadales","Fusobacteriales","Desulfovibrionaceae","Akkermansia","Parasutterella","Sutterella")
  b<-c("Lactobacillales","Bacillales","Corynebacteriales","Micrococcales","Coprobacillus","Aeromonadales","Enterobacteriaceae","Pasteurellaceae","Acinetobacter","Neisseriaceae")
 
 adi=c()
 
for ( i in 1:nrow(dat)) {
  total.b=0
  total.g=0
  for (j in 1:length(b)) {
   x=sum(dat[i,grep(b[j],colnames(dat))])
   total.b=total.b+x
  }
  
  for (j in 1:length(g)) {
    x=sum(dat[i,grep(g[j],colnames(dat))])
    total.g=total.g+x
  }
  if (total.b==0) {total.b=2/depth}
  if (total.g==0) {total.g=2/depth}
 adi[i]=log(total.b/total.g)
# print(paste("Round", i, ",Sample",rownames(dat)[i]," , sum covered:",round(total.b+total.g,2)))
  
}
names(adi)=rownames(dat)

return(adi)
}