#' Build linear model per feature
#' 
#' Run linear model for each microbial taxon (like masslin but better)
#'
#'
#'
#' @param keyobj a dataframe of metadata; sequencing sample identifiers in collumn "ID"
#' @param datobj a matrix of taxa absolute/relative abundances; sequencing sample identifiers are in row.names. Rare taxa should be removed.
#' @param fixed.vars character vector of predictors for model (names of relevant columns in keyobj)
#' @param vars.to.scale chracter vector of names of fixed.vars that need to be scales beforemodelling (usually any numerical variable)
#'
#' @return table of all coefficnts for each microbial feature
#'
#'
#' @export

#Still nder construction - investigatig corerct p-value adjustment;  for now go with q, NOT q2

lm_loop<-function(keyobj, datobj, fixed.vars, vars.to.scale="none") {
   datt<-td(datobj,transform.method = "log2")
   if (vars.to.scale[1]!="none") {
       keyobj[ ,vars.to.scale]<-scale(keyobj[ ,vars.to.scale])
   }
  dfa<-merge(keyobj, datt, by.x="ID",by.y="row.names")

  tax.names<-colnames(datt)
  res<-list()
  for (i in 1:length(tax.names)){

    # create teh function for lm:
      form<-paste(tax.names[i],paste(fixed.vars, collapse = " + "),sep="~")

      summary(lm(form, data=dfa))->m
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
    temp2$`Pr(>|t|)`<-round(temp2$`Pr(>|t|)`,5)
    temp2$q2<-round(p.adjust(temp2$`Pr(>|t|)`, "fdr"),5)#Masslin correction - I think this is WRONG!
    temp2<-temp2[order(temp2$q),]
    return(temp2)


    }






