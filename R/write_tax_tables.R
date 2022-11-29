
#' Write Summarized Taxonomy RA Tables
#'
#' write out taxonomy data tables summarized for each  taxonomy rank (phyla to species)
#'
#'
#' @param taxa dataframe of asvID and tax affilations
#' @param rawdat.ra dataframe of ASVs abundances, alraedy filtered and rariefied
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
                        phylo.levels=c("phylum","order","family","genus","species"), pathToOutdir) {


  for ( i in 1:length(phylo.levels)) {
    tax.level=phylo.levels[i]
  x<-as.data.frame(t(datobj),stringsAsFactors=F)
  temp<-rownames(x)
  if (tax.level=="genus") {
    x$taxa<-taxobj$longname.gen[match(temp, taxobj$asvID)]
  }
  if (tax.level=="species") {
    x$taxa<-taxobj$longname.sp[match(temp, taxobj$asvID)]
  }

  if (tax.level=="family") {
    x$taxa<-taxobj$longname.fam[match(temp, taxobj$asvID)]
  }

  if (tax.level=="order") {
    x$taxa<-taxobj$longname.ord[match(temp, taxobj$asvID)]
  }

  if (tax.level=="class") {
    x$taxa<-taxobj$longname.cla[match(temp, taxobj$asvID)]
  }

  if (tax.level=="phylum") {
    x$taxa<-taxobj$longname.phy[match(temp, taxobj$asvID)]
  }

  xagg<-aggregate(x[ ,-ncol(x)],by=list(x$taxa),FUN=sum)
  colSums(xagg[ ,-1])
  rownames(xagg)<-xagg[ ,1]
  xagg<-xagg[ ,-1]
  dat<-t(xagg) #dat noew again a dtandard dat object with
  dat<-decostand(dat, "total")
  dat<-round(dat,5)
  write.csv(dat,paste0(pathToOutdir,"/",tax.level,".csv"))
  }



}
