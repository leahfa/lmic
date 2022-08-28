#' Correctly Format Metadata Dates
#'
#' Change metadata columns which have "date" in name  to 'date' format
#' @param keyobj input metadata dataframe
#' @param informat format of input chracter string, defalt is "%d/%m/%Y"
#'
#'
#' @return key with all date columns formatted as Date
#' @export
#'
#'

Change.to.date<-function(keyobj, in.format="%d/%m/%Y") {
  x=grep("date",colnames(keyobj),ignore.case =T)
for ( i in (1:length(x))) {
  keyobj[ ,x[i]]<-gsub("\xa0","", keyobj[ ,x[i]]) #fix of a libreoffice bug
  keyobj[ ,x[i]]=as.Date(keyobj[ ,x[i]],in.format)
}
  return(keyobj)
}

