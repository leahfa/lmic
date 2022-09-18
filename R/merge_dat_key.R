#' Make Dataframe of Metadata and Microbiome Data
#'
#' Combine taxa abundances/relative abundances and metadata
#'
#'
#'
#' @param keyobj a dataframe of metadata; sequencing sample identifiers in collumn "ID"
#' @param datobj a matrix of taxa absolute/relative abundances; sequencing sample identifiers are in row.names
#' @param id variable in key  to merge on
#'
#' @return a dataframe with seqeunce identifiers in "ID" column
#'
#'
#' @export


mrdk<-function(keyobj, datobj,id="ID") {
  dall<-merge(keyobj, datobj, by.x=id, by.y="row.names")
}
