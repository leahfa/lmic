#' Make UniFrac matrices
#'
#'
#' Make abundance weighted/unweighted  distance matrices using UniFrac method
#'
#'
#' @param taxa dataframe of asvID and tax affilations 
#' @param datobj dataframe of ASVs abundances, alraedy filtered and rariefied
#' @param outgroup partial name of phylogentic group to root on; "Methano" selected by default. If more than 1 ASV is 
#' found in this taxonomial group, the first one will be used as the outgroup
#' @param pathToOutdir optional - specifcy path to directory in which unifrac weighted and unwieghted matrices will be written
#'
#'
#' @return a list of weighted and unweighetd UniFrac matrices
#'
#'
#' @export
#' 
#' 
make_unifrac<-function(taxobj,rawdatobj=rarified.lis[[2]],outgroup="Methano",pathToOutdir=NULL){
tax.f<-taxobj[which(taxobj$asvID  %in% colnames(rawdatobj)),]
  seqs<-DNAStringSet(tax.f$seq)
names(seqs)<-tax.f$asvID
ms<-msa(seqs)
ms.for.seqinr<-msaConvert(ms, type="seqinr::alignment")
d <- dist.alignment(ms.for.seqinr, "identity")
msTree <- nj(d)
phtree<-as.phylo(msTree)
asv.outgroup<-tax.f$asvID[grep(outgroup,tax.f$longname.gen)[1]]
phtree=root(phtree, outgroup=asv.outgroup, resolve.root = TRUE)
dis.uni<-GUniFrac(rawdatobj,phtree)$unifracs
unweighted.uni<-dis.uni[, ,"d_UW"]
weighted.uni<-dis.uni[, , "d_1"]
write.csv(weighted.uni, paste0(pathToOutdir,"weighted_unifrac.csv"))

write.csv(unweighted.uni, paste0(pathToOutdir,"unweighted_unifrac.csv"))
lis<-list(weighted.uni,unweighted.uni)
names(lis)<-c("weighted.uni","unweighted.uni")
return(lis)
}
