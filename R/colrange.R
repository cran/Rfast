

colrange <- function(x, cont = TRUE){
	x <- Rfast::col_Mins_Maxs(x)
	x <- x[2,]-x[1,]
	if(!cont){
  		return (x + 1)
  	}
  	x
}