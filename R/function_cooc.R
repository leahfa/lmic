
#' Get coccourence Stats
#'
#' Get Stats per ASV across rarefaction depths and iterations
#'
#'
#'
#'
#' @param dfx  a dataframe with all proportion and counts of each ASV across sample types. 
#' each row represents one depth in one iteratio for one ASV
#' 
#' @param asvx ASV i want to get stats on
#'
#'
#' @return dfres aa datafreme , per ASV , giving: lowest depth it was found in, number depths it was found in, 
#' num times it was found (all iterations on all dephts), mean RA and mean prop for fecal and oral. Note those are calculated only
#' from rows wehre it was found (i.e., number fo depths/itetations it was not found dont play a part)
#' @export


get_cooc_stats<-function(asvx,dfall) {
  stats=dfall %>%
    ungroup() %>%
    filter(asvID==asvx) %>%
    summarize(
  min.depth=min(Depth),
  num.depths=n_distinct(unique(Depth)),
  total.count=n(),
  mean.oral.only.prop=mean(prop.oral.only),
  mean.fecal.only.prop=mean(prop.fecal.only),
  mean.both.prop=mean(prop.both),
  mean.num.both=mean(n.both),
  mean.oral.RA=mean(mean.oral.ra),
  mean.fecal.RA=mean(mean.fecal.ra),
  
  )
  stats$asvID<-asvx
    return(stats)
  
  
  
}


get_cooc_by_pat_stats<-function(asvx,dfall) {
  stats=dfall %>%
    ungroup() %>%
    filter(asvID==asvx) %>%
    summarize(
      min.depth=min(Depth),
      num.depths=n_distinct(unique(Depth)),
      total.count=n(),
      mean.oral.only.prop=mean(prop.oral.only),
      mean.fecal.only.prop=mean(prop.fecal.only),
      mean.both.prop=mean(prop.both),
      mean.num.both=mean(n.both),
      mean.oral.RA=mean(mean.mean.oral.ra),
      mean.fecal.RA=mean(mean.mean.fecal.ra),
      
    )
  stats$asvID<-asvx
  return(stats)
  
  
  
}