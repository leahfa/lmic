#' Summarize results of lm_loop across subcohorts
#' 
#' 
#' Run the same lm_loop on different cohorts or subsets; combine teh ersults of all subsets/cohorts
#'  
#'
#'
#'
#' @param keyobj a dataframe of metadata; sequencing sample identifiers in collumn "ID"
#' @param datobj a matrix of taxa absolute/relative abundances; sequencing sample identifiers are in row.names. Rare taxa should be removed.
#' @param extra.vars character vector of additional predictors for model (names of relevant columns in keyobj)
#' @param yfv name of variable am inetersted in 
#' @param cohorts cahacter vector of names of subcohorts
#' @return table of y
#'
#'
#' @export

compare_lm_loops<-function(keyobj, datobj , cohorts, yfv, extra.vars,... ) {
  fixed.vars<-c(extra.vars, yfv)
  #key<-key[which(is.na(key$treat.Biol)==FALSE),]
  datf<-filtColsTmax(subdat(keyobj,datobj),15,0.02)
  res<-lm_loop(keyobj, datf,fixed.vars)
  res.all<-res[grep(yfv, res$coefficient),]
  res.all<-res.all[order(res.all$q),]
  
  #compare to each dataset alone:
  ####Biobank ####
  key.s1<-keyobj[which(keyobj$Project==cohorts[1]),]
  datf<-filtColsTmax(subdat(key.s1,datobj),5,0.005)
  res<-lm_loop(key.s1, datf,fixed.vars)
  res1<-res[grep(yfv, res$coefficient),]
  res.name1<-(paste0("res.",cohorts[1]))
  assign(res.name1,res1)
  ####And for RMC ####
  key.s1<-keyobj[which(keyobj$Project==cohorts[2]),]
  datf<-filtColsTmax(subdat(key.s1,datobj),5,0.005)
  res<-lm_loop(key.s1, datf,fixed.vars)
  res1<-res[grep(yfv, res$coefficient),]
  res.name2<-(paste0("res.",cohorts[2]))
  assign(res.name2,res1)
  ###combine results####
  temp<-merge(res.name1[ ,c(1,4,5,7)], res.name2[ , c(1,4,5,7)], by="Taxon")
  sumall<-merge(res.all[ , c(1,4,5,7)], temp, by="Taxon")
  sumall<-sumall[order(sumall$q),]
  colnames(sumall)[2:4]<-paste("Merged", colnames(sumall)[2:4], sep="\n")
  colnames(sumall)[5:7]<-paste(cohorts[1], colnames(sumall)[5:7], sep="\n")
  colnames(sumall)[8:10]<-paste(cohorts[2], colnames(sumall)[8:10], sep="\n")
  colnames(sumall)<-gsub(".x","",colnames(sumall), fixed=TRUE)
  colnames(sumall)<-gsub(".y","",colnames(sumall),fixed=TRUE)
  final.name<-(paste0("sumall.",yfv))
  assign(final.name,sumall)
  return(final.name)
}
