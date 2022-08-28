#' Match Microbiome and Metadata
#'
#'
#' Matches data (datobj) and metadata (keyobj) so both have same samples in same order
#' If input data is a distance matrix, will match both rows and columns
#' @param keyobj  metadata key; sampels in rows;  column of sample sequncing identifiers
#' called "ID"
#' @param datobj  numerical matrix or dataframe; Default is 'tax.table'  - a RA
#' or abundance table with samples in rows; can also accept UniFrac distance matrix.
#' Sample seqeunce identifiers must be in rownames (and if its UniFrac,also in colnames)
#' @param kind kind of data input - RA/abundance table ('tax.table' , default) or UniFrac
#' distance matrix ('unifrac')
#' @return a data object perfectly matched to key
#' @export




subdat<-function(keyobj,datobj,kind="tax.table") {
  #keep<-intersect(rownames(dat),key.s$ID) #functon only returns one object
  #key.s<-key.s[which(key.s$ID %in% keep),] #so if changing key as well as datobj, need to retirn both objects as lists
  keep<-intersect(rownames(datobj),keyobj$ID)
  print(paste("number of samples (including reruns and replicates) in both key and dat:", length(keep)))
  if (length(keep)<nrow(keyobj)) {print("Attention  - key has samples missing from dat")}
  keyobj<-keyobj[which(keyobj$ID %in% keep),]
  if (kind=="unifrac") {

  dat.s=datobj[match(keyobj$ID,rownames(datobj)),match(keyobj$ID,colnames(datobj))]
  message(identical(rownames(dat.s),keyobj$ID))
  message(identical(colnames(dat.s),keyobj$ID))
}  else {
  dat.s=datobj[match(keyobj$ID,rownames(datobj)),,drop=FALSE]
  all.equal(rownames(dat.s),keyobj$ID)->x
  if (x==TRUE) {message("dat and key perfectly matched")} else {message("Mayday!dat and key dont match")}

}

 return(dat.s)
}

