#' Show Available Hues of Color
#'
#' Show available hues for any particular color-helps choose colors for plots!
#'
#'
#'
#' @param name of a color
#' 
#' 
#'
#' @return a piechart of avaialble colors

#' @export




color.chooser<-function(x){
###choose start and  stop shades for colors:
cl=colors() #a vector of all 657 colors defined by name in r
grep(x,cl)->y
p<-pie(1:length(y),col=cl[y],labels=cl[y]) #shows a pie of defined shades of 'x'
return(p)

}