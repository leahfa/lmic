

#' Shorten Full-Taxonomy Names
#'
#' Cut taxa names to the required phylogenetic level.
#' Taxa not assigned to the requested level are assigned the
#' deepest taxonomic level available.
#' Special characters in taxon names are replaced.
#'
#' @param x Character vector of taxonomy names, or a sequence table
#'   (matrix/data frame) with taxa as columns.
#' @param t Taxonomic rank: 1 = phylum, 2 = class, 3 = order,
#'   4 = family, 5 = genus, 6 = species.
#' @return A vector of shortened names, or the original sequence
#'   table with shortened column names.
#' @export

strip.names.silva.dada <- function(x, t) {

  # NEW: accept sequence table or character vector
  is.table <- is.matrix(x) || is.data.frame(x)

  if (is.table) {
    dat <- x
    x <- colnames(dat)
  }

  # ORIGINAL FUNCTION - UNCHANGED

  lev=c(";D_1__", ";D_2__" ,";D_3__" ,";D_4__", ";D_5__" ,";D_6__")

  temp=strsplit(x,lev[t],fixed=TRUE)
  temp1=unlist(lapply(temp, "[",2))
  new.names=rep("blank",length(temp1))

  for (i in 1:length(temp1)){
    z=t-1
    if (is.na(temp1)[i]==F & temp1[i]!="NA")  {
      new.names[i]=temp1[i]
    } else {
      y=temp[i][[1]]
      for (j in 1:(t-1)) {
        temp2=unlist(strsplit(y,lev[z],fixed=TRUE))[2]
        temp3=unlist(strsplit(y,lev[z],fixed=TRUE))[1]
        if (is.na(temp2)==F & temp2!="NA") {
          new.names[i]=temp2
          break
        } else {
          z=z-1
          y=temp3
        }
      }
    }
  }

  new.names=gsub("[","",new.names,fixed=T)
  new.names=gsub("]","",new.names,fixed=T)
  dups=which(summary(as.factor(new.names),maxsum = 500)>1)
  for (i in 1:length(dups)) {
    d=dups[i]
    ind=which(new.names==names(d))
    add=1
    for (j in 1:length(ind)){
      helper=ind[j]
      new.names[helper]=paste0(new.names[helper],"_",add)
      add=add+1
    }
  }

  new.names<-gsub("-","_",new.names, fixed=TRUE)
  new.names<-gsub("/",".",new.names,fixed=TRUE)
  new.names<-gsub("|",".",new.names,fixed=TRUE)
  new.names<-gsub(" ","_",new.names,fixed=TRUE)

  # NEW: return table if input was a table
  if (is.table) {
    colnames(dat) <- new.names
    return(dat)
  }

  return(new.names)
}







#' Old version of strip.names.silva.dada
#' @noRd
#' Cut taxa names to required phylogebnetic level only.
#' NOTE:Taxa that are not assigned down to the required level will get be assigned the
#'  deepest level of taxonomny they DO havE! Special character in taxa names are changed
#'  to dors or underscores
#
#' @param x vector of taxa names that needs to be cut
#' @param t taxonmic rank of interest: 1 is phyla, 2 order, and so on
#' @return vector of names cut down to rank of interest
#' @export






strip.names.silva.dada.old<-function(x,t)   {

  lev=c(";D_1__", ";D_2__" ,";D_3__" ,";D_4__", ";D_5__" ,";D_6__") #Vector of characters to split on-need to cycle through this as we go up through levels

  temp=strsplit(x,lev[t],fixed=TRUE)
  temp1=unlist(lapply(temp, "[",2)) #create a vector showing only the required portion of each name; will be NULL if taxonomic assignemnt didnt reach that deep
  new.names=rep("blank",length(temp1))
  #cycle through temp1, names are assigned to new.names, and Nulls go into the 'else' part of an 'if' loop
  for (i in 1:length(temp1)){
    z=t-1 #set this as a starting point for the next loop  - it needs to start cycling one level above the original split
    if (is.na(temp1)[i]==F & temp1[i]!="NA")  {new.names[i]=temp1[i]} else {
      y=temp[i][[1]] #the string that needs to be broken down again (on the next-level chracter)
      for (j in 1:(t-1))   {
        temp2=unlist(strsplit(y,lev[z],fixed=TRUE))[2] #If its not NULL - thats the name we want right there
        temp3=unlist(strsplit(y,lev[z],fixed=TRUE))[1] #If temp2 IS NULL, this should be the input for the next iteration of splitting
        if (is.na(temp2)==F & temp2!="NA")  {
          new.names[i]=temp2 #grab the name, break out of the loop, move on to next 'i' in temp1
          break
        } else {
          z=z-1 #need to go up to the next character to split on (one taxonomic level higher)
          y=temp3 #this is so the next round will use the product of this splitting round (temp3) as template
        }

      }
    }
  }
  new.names=gsub("[","",new.names,fixed=T) #remove junk brackets from names
  new.names=gsub("]","",new.names,fixed=T)
  dups=which(summary(as.factor(new.names),maxsum = 500)>1)
  for (i in 1:length(dups)) { #give unique names to duplicate names
    d=dups[i]
    ind=which(new.names==names(d))
    add=1
     for (j in 1:length(ind)){
       helper=ind[j]
       new.names[helper]=paste0(new.names[helper],"_",add)
       add=add+1
     }
  }
  new.names<-gsub("-","_",new.names, fixed=TRUE)
  new.names<-gsub("/",".",new.names,fixed=TRUE)
  new.names<-gsub("|",".",new.names,fixed=TRUE)
  new.names<-gsub(" ","_",new.names,fixed=TRUE)


  return(new.names)
}
