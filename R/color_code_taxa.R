#' Assign Specific Colors to Lineages
#' 
#' Assign color to taxa based on phylogenetic lineage and according to a specific 
#' pre-loaded taxa to colors key- this must be loaded into your environment and called
#' `cod`. 
#' 
#' @param tax a vector of full-lineage taxa names 
#' @param dattable a datobj object
#'
#'
#' @return a named character vector of color names; taxa names are the names of the vector 
#' 
#' @export







#Make sure you have a color-coding file loaded (into "cod") before running this
color_code_taxa=function(tax,dattable) {
  tax=sort(tax)
  col.tax=c()
  counter=1
  for (i in 1:nrow(cod)) {
    x=grep(cod$V1[i],tax)
    if (length(x)==1) {col.tax[counter]=cod$V2[i];names(col.tax)[counter]=tax[x];counter=counter+1}
    if (length(x)>1) {
      n=length(x)
      
      y=colorRampPalette(c(cod[i,2],cod[i,3]))(n)
     # pie(1:length(y),col=y,main=i,labels=tax[x])
      
      col.tax[counter:(counter+n-1)]=y
      names(col.tax)[counter:(counter+n-1)]=tax[x]
      counter=counter+n
    } 
    if (length(x)==0) {
      print(paste0("no match",i))
    }
    
  
  
  }
  z<-setdiff(colnames(dattable),names(col.tax))
  if (length(z)>0) {
  n<-length(col.tax)
  for ( i in 1:length(z)) {
  col.tax[n+i]<-"antiquewhite"
  names(col.tax)[n+i]<-z[i]
  }
  }
  return(col.tax)
  }
  