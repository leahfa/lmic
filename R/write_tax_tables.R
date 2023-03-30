
#' Write Summarized Taxonomy RA Tables
#'
#' write out taxonomy data tables summarized for each  taxonomy rank (phyla to species)
#'
#'
#' @param taxa dataframe of asvID and tax affilations
#' @param rawdat.ra dataframe of ASVs abundances, alraedy filtered and rariefied
#' @param strip strip taxa names to one loevel (default: no, set "yes" to strip)
#' @param transpose transpose data (default: no, set "yes" to transpose)
#' @param phylo.levels optional; specify phyl levels to write out; by defualt all will be selected
#' @param pathToOutdir specify path to directory in which taxonomy tables will be written
#'
#'
#'
#'
#'
#' @export
#'
#'

write.tax.tables<-function(taxobj, datobj=rarified.lis[[2]],
                           strip="no",transpose="no",
                        phylo.levels=c("phylum","order","family","genus","species"), pathToOutdir) {


  for ( i in 1:length(phylo.levels)) {
    tax.level=phylo.levels[i]
  x<-as.data.frame(t(datobj),stringsAsFactors=F)
  temp<-rownames(x)
  if (tax.level=="genus") {
    x$taxa<-taxobj$longname.gen[match(temp, taxobj$asvID)]
    tl=5
  }
  if (tax.level=="species") {
    x$taxa<-taxobj$longname.sp[match(temp, taxobj$asvID)]
    tl=6
  }

  if (tax.level=="family") {
    x$taxa<-taxobj$longname.fam[match(temp, taxobj$asvID)]
    tl=4
  }

  if (tax.level=="order") {
    x$taxa<-taxobj$longname.ord[match(temp, taxobj$asvID)]
    tl=3
  }

  if (tax.level=="class") {
    x$taxa<-taxobj$longname.cla[match(temp, taxobj$asvID)]
    tl=2
  }

  if (tax.level=="phylum") {
    x$taxa<-taxobj$longname.phy[match(temp, taxobj$asvID)]
    tl=1
  }

  xagg<-aggregate(x[ ,-ncol(x)],by=list(x$taxa),FUN=sum)
  colSums(xagg[ ,-1])
  rownames(xagg)<-xagg[ ,1]
  xagg<-xagg[ ,-1]
  dat<-t(xagg) #dat noew again a dtandard dat object with
  dat<-decostand(dat, "total")

  
  dat<-round(dat,5)
  if (strip=="yes") {
    colnames(dat)<-strip.names.silva.dada(colnames(dat),tl)
  }
  if (transpose=="yes") {
    dat<-t(dat)
  }
  write.csv(dat,paste0(pathToOutdir,"/",tax.level,".csv"))
  }



}
