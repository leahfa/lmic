#' Make a cross-section of longitudinal data
#' 
#' Make a longitudinal metadata key into a cross sectional one - by choosing
#' one sample per subject.NOTE: Function assumes the column with subject identity is
#' called "PatID", and that it is sorted by date

#' @param keyobj  metadata key; subject column called "PatID"
#' @param index "first" or "last" - specify which  sample should be taken. This 
#' only works if teh key is ordered by date first!
#' @return a cross sectional metadata key
#' @export



make_cross_section<-function(keyobj,index) {
keyobj$PatID=as.factor(keyobj$PatID)
sum=summary(keyobj$PatID,maxsum = 1000)
#### then run loop to choose 1 sample per patinet:
num.per.pat=c()
name.pat=c()
csKey=list()
pat=levels(keyobj$PatID)
for (i in 1:length(pat)) {
  y=which(keyobj$PatID==pat[i])#find indexes of all row belonging to patinet i. For each patient, will be in order determined above by diag.tm!
 if (index=="first") {ind<-1}
  if (index=="last") {ind<-length(y)}
   x=y[ind]  
   csKey[[i]]=keyobj[x,]
    
  num.per.pat[i]=length(y)
  name.pat=pat[i]
} 
samples.per.pat=cbind(name.pat,num.per.pat)  
key.c=do.call("rbind",csKey) 
return(key.c)
}


make_cross_section.samp<-function(keyobj,index) {
  keyobj$StoolID=as.factor(keyobj$StoolID)
  sum=summary(keyobj$StoolID,maxsum = 1000)
  #### then run loop to choose 1 sample per patinet:
  num.per.pat=c()
  name.pat=c()
  csKey=list()
  pat=levels(keyobj$StoolID)
  for (i in 1:length(pat)) {
    y=which(keyobj$StoolID==pat[i])#find indexes of all row belonging to patinet i. For each patient, will be in order determined above by diag.tm!
    if (index=="first") {ind<-1}
    if (index=="last") {ind<-length(y)}
    x=y[ind]  
    csKey[[i]]=keyobj[x,]
    
    num.per.pat[i]=length(y)
    name.pat=pat[i]
  } 
  samples.per.pat=cbind(name.pat,num.per.pat)  
  key.c=do.call("rbind",csKey) 
  return(key.c)
}