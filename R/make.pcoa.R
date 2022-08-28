#' Draw a PCoA
#' 
#' Make a PCoA,map dot color (and optional, shape ) to a metadata factor. If data 
#' inputted is a a RA/abundance table, it will be converted to distance object using vegdist
#' 
#' @param datobj matrix of RA/abundance data (numeric), or a Distance matrix
#' @param keyobj correspondin metadata key; sampels must be in same order!
#' @param kind type of data - `unifrac` (distance matrixes) or `taxtable` (ASVs count as taxtables )
#' @param dis distance index -  `bray` or  `jaccard` - relevant only if `kind` is taxtable
#' @param var1 variable of interest  -give actual values (i.e., key$group)
#' @param var2 optional - anotehr variable of interest  -give actual values (i.e., key$Group)
#' @param name1 name of var1 (i.e., "group")
#' @param name2 name of var2 (optional)
#' @param i number of coordinate to plot as X
#' @param j number of coortdinate to plot as y
#' @return PCoA plot withn ANOSIM statistics shown 












make.pcoa<-function(datobj,keyobj,kind, dis,var1,var2,name1,name2,i,j,...) {



if (kind =="taxtable"){
  if (dis=="bray") {X=F} else {X=T}
  dista=vegdist(datobj,method=dis,binary=X)
} else {dista=datobj}
identical(rownames(as.matrix(dista)),keyobj$ID)
####plot PCOA on subset####
co=cmdscale(dista,k=5,eig=T)
eigs=co$eig
eigs2=eigenvals(co)
exp=round(100*(abs(eigs))/(sum(abs(eigs))),1) #gives same % explained as "PAST" does
dfw=as.data.frame(co[[1]])
rownames(dfw)=gsub("X","",rownames(dfw))


forplot<-merge(dfw, keyobj,by.x="row.names",by.y="ID")
forplot<-forplot[match(keyobj$ID,forplot$Row.names),]
print(all.equal(as.character(forplot$Row.names),as.character(keyobj$ID)))

anosim(dista,as.factor(var1),999)->a1
p1=a1$signif
r1=round(a1$statistic,2)
print(paste("anosim", "p=", p1,"| R=", r1))

pl<-ggplot(forplot, aes(x=forplot[ ,i+1],y=forplot[ ,j+1],color=as.factor(var1), shape=as.factor(var2)))+
  #geom_text(size=(3),vjust=-0.5,hjust=-0.5)+
  geom_point(  size=5)+  
  #geom_point(aes(size=t))+
  # scale_color_manual(values=c(pal[1],pal[3],pal[5]),labels=legend.names)+ #,pal[4],pal[8]))+
  #scale_colour_gradientn(colours = brewer.pal(9,"BrBG")[c(8:9,1:3)])
  #scale_size_continuous(range=c(3,7))+
  # geom_point(aes(size=varT))+
  labs(x=paste0("Coordinate ",i," ",exp[i],"%"),y=paste0("Coordinate ",j," ",exp[j],"%"),
       color=name1,shape=name2)+
  labs(caption=paste(kind," ", dis," ","p ",name1,"=",p1," R=",r1,sep=" "))
return(pl)
}
