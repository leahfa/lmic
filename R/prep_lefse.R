#' Make LEfSe Input Table
#'
#' Make table formatted to serve as input for a LEfSe analysis, comparing 2 groups based on
#' one metadata factor
#'
#'
#'
#' @param keyobj a dataframe of metadata; sequencing sample identifiers in collumn "ID"
#' @param datobj a matrix of taxa absolute/relative abundances; sequencing sample identifiers are in row.names. Rare taxa should be removed.
#' @param varname varaible to compare groups on
#' @param dirname path to store comparison directory
#' @param dirname directory of comparison
#'
#'
#'
#' @export
prep.lefse<-function(keyobj, datobj, varname, path, dirname){

  dir.create(paste0(path,"lefse/"))


dat.s<-filtCols(subdat(keyobj, datobj),0)
pathl=paste0(path,"lefse/", varname,"/")
dir.create(pathl)
for.lefse<-t(dat.s)
all.equal(colnames(for.lefse),keyobj$ID)
colnames(for.lefse)<-keyobj[ , which(colnames(keyobj)==varname)]
final<-filt.lefse(for.lefse,unique(colnames(for.lefse)))
write.table(final, paste0(pathl,"/L6.txt"),sep="\t",quote=FALSE,row.names=FALSE)
}

