#' Shorten Full-Taxonomy Eukaryal Names
#'
#' Cut taxa names to required phylogenetic level only.
#' NOTE:Taxa that are not assigned down to the required level will be assigned the
#'  deepest level of taxonomny they DO have! Special character in taxa names are
#'   changed to dots or underscores
#
#' @param x vector of taxa names that needs to be cut
#' @param t taxonmic rank of interest: 1 is phyla, 2 order, and so on
#' @return vector of names cut down to rank of interest
#' @export




strip.names.silva.fungal.fixed<-function(x,t)   {
  lev=c(";D_1__", ";D_2__" ,";D_3__" ,";D_4__", ";D_5__" ,";D_6__",";D_7__",";D_8__",";D_9__",";D_10__",";D_11__",";D_12") #Vector of characters to split on-need to cycle through this as we go up through levels

  temp=strsplit(x,lev[t],fixed=TRUE)
  temp1=unlist(lapply(temp, "[",2)) #create a vector showing only the required portion of each name; will be NULL if taxonomic assignemnt didnt reach that deep
  new.names=rep("blank",length(temp1))
  #cycle through temp1, names are assigned to new.names, and Nulls go into the 'else' part of an 'if' loop
  for (i in 1:length(temp1)){
    z=t-1 #set this as a starting point for the next loop  - it needs to start cycling one level above the original split
    if (is.na(temp1)[i]==F & temp1[i]!="Other" & temp1[i]!="uncultured" ) { new.names[i]=temp1[i]} else {


      y=temp[i][[1]] #the string that needs to be broken down again (on the next-level chracter)
      for (j in 1:(t-1))   {
        temp2=unlist(strsplit(y,lev[z],fixed=TRUE))[2] #If its not NULL - thats the name we want right there
        temp3=unlist(strsplit(y,lev[z],fixed=TRUE))[1] #If temp2 IS NULL, this should be the input for the next iteration of splitting
        if (is.na(temp2)==F & temp2 !="Other" & temp2!="uncultured")  {
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

  return(new.names)
}
