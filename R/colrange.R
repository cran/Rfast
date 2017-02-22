

colrange <- function(x, cont = TRUE){
	x <- Rfast::colMinsMaxs(x)
	x <- x[2,]-x[1,]
	if(!cont){
  		return (x + 1)
  	}
  	x
}