 
#' Identify  R.gnavus Sequences
#' 
#' This taxonomy is not given automatically by dada2. this function finds all sequnences which may be 
#' R.gnavus (Unassigned Lachnospiraceae) and writes them out as a fasta file which can be explored by 
#' blast to which ASVs of Unassigned Lachnospiraceaeare are actually  R. gnavus
#'
#' @param taxaobj taxa table
#' @param path to write output
#' 
#'
#'
#' @export
#'
mine.gnavus.1<-function(taxaobj, path.out) {
temp<-DNAStringSet(taxaobj$seq[which(taxaobj$Family=="Lachnospiraceae" & is.na(taxaobj$Genus))])
names(temp)<-taxaobj$asvID[which(taxaobj$Family=="Lachnospiraceae" & is.na(taxaobj$Genus))]
print(paste(length(temp),"unassigned Lachnospiraceae seqs"))
writeXStringSet(temp,paste0(path.out,"Lacho_family_genusNA.fas"))
}

mine.gnavus.2<-function(taxaobj, path.out) {
  temp<-DNAStringSet(taxaobj$seq[which(taxaobj$Family=="Lachnospiraceae")])
  names(temp)<-taxaobj$asvID[which(taxaobj$Family=="Lachnospiraceae")]
  print(paste(length(temp),"unassigned Lachnospiraceae seqs"))
  writeXStringSet(temp,paste0(path.out,"Lacho_family.fas"))
}
