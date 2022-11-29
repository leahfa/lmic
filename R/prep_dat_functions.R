
#' Read In SeqTable and Metadata Key
#'
#' Read in a dada seqtable (with columns labelled as ASVs rather than raw sequences)
#' and corresponding key; match data and key (same samples in same order for both objects)

#' @param pathToKey   pathway to metadata key (sequencing sample identifers in
#' column "ID", samples in rows)
#' @param pathToDat  pathway to abundance data ( sequencing sample identifers
#' in row.names)
#' @return list of key and dat, properly matched; prints out to console sample numbers;
#' warns if sample names dont match. If dat has sampels missing from key, they will be dropped.
#' @export



get_data<-function(pathToKey,pathToDat) {
  keyobj<-read.csv(pathToKey, stringsAsFactors =F)
  datobj<-read.table(pathToDat, sep="\t", check.names = FALSE)
  keep<-intersect(rownames(datobj),keyobj$ID)
  length(keep)
  setdiff(rownames(datobj),keep)
  setdiff(keyobj$ID,keep)
  keyobj<-keyobj[which(keyobj$ID %in% keep),]
  table(keyobj$Project)
  datobj<-subdat(keyobj, datobj,"L7")
  list.obj<-list(keyobj,datobj)
  names(list.obj)<-c("key","data.abundance.table")
  return(list.obj)
}


#' Read In and Format Taxonomy
#'
#' Read in the matching dada taxa table, which has an asvID column matching dat column names;
#' Also read in a blast results file for R.gnavus, iif annotation correction is necessary (
#'  default is "None" ). add to taxa 6 columns corresponding to full phylogenetic lineage, down
#' Phyla, order, class, family, genus, and species. Silva database conventin used to specifcy
#' taxonmoy ranks (D_1__; D_2__; etc.)

#' @param pathToTaxa   pathway to taxa table
#' @param pathToGnavus  pathway to blast file, if exists
#' @return taxa object with full names added in; If a blast file is specified,
#'  R. gnavus annotations are corrected
#' @export

get_taxa<-function(pathToTaxa,pathToGnavus="None"){
  taxaobj<-read.csv(pathToTaxa,stringsAsFactors= F,header=T)
  if (pathToGnavus!="None")   {
  bl<-read.table(pathToGnavus,sep="\t",header=FALSE,stringsAsFactors = FALSE)
  bl.f<-bl[which(bl$V3>98 & bl$V4>240),]
  gna.index<-which(taxaobj$asvID %in% bl.f$V2)
  taxaobj$Genus[gna.index]<-"Ruminococcus_gnavus"
  print("gnavus successfully assigned to the following ASVs:")
  print(taxaobj$asvID[gna.index])

  }
  #taxobj$Species[gna.index]<-"gnavus"
  taxaobj$longname.phy<-paste0("D_0__",taxaobj$Kingdom,";D_1__",taxaobj$Phylum)
  taxaobj$longname.cla<-paste0("D_0__",taxaobj$Kingdom,";D_1__",taxaobj$Phylum,";D_2__",taxaobj$Class)
  taxaobj$longname.ord<-paste0("D_0__",taxaobj$Kingdom,";D_1__",taxaobj$Phylum,";D_2__",taxaobj$Class,";D_3__",taxaobj$Order)

  taxaobj$longname.fam<-paste0("D_0__",taxaobj$Kingdom,";D_1__",taxaobj$Phylum,";D_2__",taxaobj$Class,";D_3__",taxaobj$Order,
                               ";D_4__",taxaobj$Family )

  taxaobj$longname.gen<-paste0("D_0__",taxaobj$Kingdom,";D_1__",taxaobj$Phylum,";D_2__",taxaobj$Class,";D_3__",taxaobj$Order,
                               ";D_4__",taxaobj$Family,";D_5__",taxaobj$Genus)
  taxaobj$longname.sp<-paste0("D_0__",taxaobj$Kingdom,";D_1__",taxaobj$Phylum,";D_2__",taxaobj$Class,";D_3__",taxaobj$Order,
                              ";D_4__",taxaobj$Family,";D_5__",taxaobj$Genus,";D_6__",taxaobj$Species)
  return(taxaobj)
}


#' Sum Duplicates
#'
#' Sum duplicates, if exists (samples which were sequenced in more than one run. They will have
#' same sample name , but unique sample sequencing
#' identifiers (ID column in key))

#' @param dat.lis  A list of key and dat,perfectly matched; may be prepared with
#' get_data()
#'
#' @return A list of key, and taht, still perfectly matched, but abundances of multiple
#' runs of same sampel will be summed to one entry
#' @export
sum_duplicates<-function(dat.lis,sample.id="SampleID") {
  keyobj<-dat.lis[[1]]
  datobj<-dat.lis[[2]]
  keyobj<-keyobj[order(keyobj$ID), ]
  datobj<-subdat(keyobj,datobj,kind="tax.table")
  sampind=which(colnames(keyobj)==sample.id)
  z<-(table(keyobj[ ,sampind]))
  z1<-names(z[which(z>1)])
  print("These are sampels names of samples with duplicates  which will be summed:")
  print(z1)
  checkup=c()
  #keyobj<-keyobj[order(keyobj$ID, decreasing=T),]

  print("summing duplicates...this may take a few minutes")
  for (i in 1:length(z1)){
    ind<-which(keyobj[ ,sampind]==z1[i])
    # print(length(ind))
    temp<-colSums(datobj[ind,]) #create a vector summing the dupliactes
    datobj[ind[1],]<-temp #the 1st row of sampel will be kept with the sum
    datobj<-datobj[-ind[-1],] #remove from datobj the other rows
    #re-subset keyobj to fit datobj:
    keyobj<-keyobj[-ind[-1],]
    checkup[i]<-all.equal(rownames(datobj),keyobj$ID)
  }

  x<-all(checkup)

  if (x==TRUE) {print("sampels successfully summed")} else {
    print("Mayday! The following sampels didnt sum properly:")
    print(z1[which(checkup==FALSE)]) }
  return(list( keyobj, datobj))
}


#' Filter  Data
#'
#'
#' Remove rare ASvs based on prevalence and minimal abundance
#'
#' @param dat.lis perfectly matched list of key and dat (column as ASVs, raw abundance)
#' @param x prevalence threshold;features whsoe prevalence is equal or smaller than x
#' will be dropped
#' @param tmax minimal RA/abundance threshold; features whose maximal RA/abundance is lower
#' than this are dropped (i.e., must pass this threshold in at least ONE sample to be
#' retained). Note this step is run AFTER the prevalence filtration.
#' @return dat.lis where the data object is filtered from rare ASVs
#' @export



filt_data<-function(dat.lis,x,tmax) {
  keyobj<-dat.lis[[1]]
  datobj<-dat.lis[[2]]
  datobj.filt<-filtColsTmax(datobj,x,tmax)
  print("% sequenecs left after filtration (showing 25 lowest sampels):")
  print(sort(rowSums(datobj.filt)/rowSums(datobj))[1:25])
  print("summary of % sequences left after filtration:")
  print(summary(rowSums(datobj.filt)/rowSums(datobj)))
  print("summary number of sequences left after filtration (sample depth):")
  print(summary(rowSums(datobj.filt)))
  print(paste("Number of ASVs left after filtration:", ncol(datobj.filt), " out of", ncol(datobj)))

  return(list(keyobj, datobj.filt))
}

#' Rarify data
#'
#' Rarefy data to a common depth. remove samples not reaching this depth from both dat and key
#'
#'
#' @param dat.lis perfectly matched list of key and dat (column as ASVs, raw abundance)
#' @param depth data wiull be rarieifed to this depth; samples with fewer seqeunes will be
#' dropped from boith dat and key; thsi will be reported in console
#'
#' @return dat.lis where the data object is filtered from rare ASVs
#' @export


rarefy_data<-function(dat.lis,depth) {
  keyobj<-dat.lis[[1]]
  datobj<-dat.lis[[2]]
  datobj.r<-rrarefy(datobj,depth)
  datobj.r<-datobj.r[which(rowSums(datobj.r)==depth),]
  keep<-intersect(rownames(datobj.r),keyobj$ID)
  keyobj<-keyobj[which(keyobj$ID %in% keep),]
  datobj.r<-subdat(keyobj, datobj.r,"tax.table")
  return(list(keyobj, datobj.r))
}


#' Summarize by Taxonomy
#'
#'
#' Summarize ASV table to required phylogenetic level (default is genus)
#'
#' @param dat.lis perfectly matched list of key and dat; by default after filtartion and rarefaction
#' @param taxobj taxa table  prepared with get_taxa()
#' @param tax.level taxonomic level required; default is genus
#' @return dat.lis with teh dat object summarized to phylogebtic rtank of interest
#' @export


tax.seq.table<-function(dat.lis=rariefied.lis, taxobj, tax.level="genus") {
  datobj<-dat.lis[[2]]
  x<-as.data.frame(t(datobj),stringsAsFactors=F)
  temp<-rownames(x)
  if (tax.level=="genus") {
  x$taxa<-taxobj$longname.gen[match(temp, taxobj$asvID)]
  }
  if (tax.level=="species") {
    x$taxa<-taxobj$longname.sp[match(temp, taxobj$asvID)]
  }
  if (tax.level=="order") {
    x$taxa<-taxobj$longname.ord[match(temp, taxobj$asvID)]
  }
  xagg<-aggregate(x[ ,-ncol(x)],by=list(x$taxa),FUN=sum)
  colSums(xagg[ ,-1])
  rownames(xagg)<-xagg[ ,1]
  xagg<-xagg[ ,-1]
  dat<-t(xagg) #dat noew again a dtandard dat object with rax info
  return(dat)
}
