#' Run mixed linear model with random effect
#' for each microbial taxon (like masslin but better)
#'
#'
#'
#' @param keyobj a dataframe of metadata; sequencing sample identifiers in collumn "ID"
#' @param datobj a matrix of taxa absolute/relative abundances; sequencing sample identifiers are in row.names. Rare taxa should be removed.
#' @param transform character string describing how to transform the data, default is log2 (alternatives are none, arcsine)
#' @param fixed.vars character vector of predictors for model (names of relevant columns in keyobj)
#' @param vars.to.scale chracter vector of names of fixed.vars that need to be scales beforemodelling (usually any numerical variable)
#' @param random.vars variables to be used for random effect
#'
#' @return table of all coefficnts for each microbial feature
#'
#'
#' @export

#Still nder construction - investigatig corerct p-value adjustment;  for now go with q, NOT q2





glmer_loop<-function(keyobj, datobj,transform="log2", fixed.vars, vars.to.scale="none",
                    random.vars) {
   if (transform=="log2") {
   datt<-td(datobj,transform.method = "log2")
   } 
  if (transform=="arcsine") {
    datt<-td(datobj,transform.method = "arcsine")
  } 
  if (transform=="none") {
    datt<-datobj
  } 
   if (vars.to.scale[1]!="none") {
       keyobj[ ,vars.to.scale]<-scale(keyobj[ ,vars.to.scale])
   }
  colnames(datt)<-make.names(colnames(datt))
  dfa<-merge(keyobj, datt, by.x="ID",by.y="row.names")

  tax.names<-colnames(datt)
  res<-list()
  for (i in 1:length(tax.names)){

    # create teh function for lm:
    
    rand<-paste0("(1|",random.vars,")")
      form<-paste(tax.names[i],paste(c(fixed.vars,rand), collapse = " + "),sep="~")
    
      summary(glmer(form, data=dfa,family="binomial"))->m
      temp<-as.data.frame(m$coefficients)
      temp$Taxon<-tax.names[i]
      temp$coefficient<-rownames(temp)
      res[[i]]<-temp
    }
    temp1=do.call("rbind",res)
   #apply p-value correction:
    res2<-list()
    for ( j in 1:length(fixed.vars)) {
   x1<-temp1[grep(fixed.vars[j],temp1$coefficient),]
  # hist(x1$`Pr(>|t|)`)
   x1$q<-round(p.adjust(x1$`Pr(>|t|)`,"fdr"),5)

   res2[[j]]<-x1

    }

    temp2<-do.call("rbind", res2)
    temp2$Estimate<-round(temp2$Estimate,3)
    temp2$q2<-round(p.adjust(temp2$`Pr(>|t|)`, "fdr"),5)#Masslin correction - I think this is WRONG!
    temp2<-temp2[order(temp2$q),]
    rownames(temp2)<-NULL
    return(temp2)


    }






