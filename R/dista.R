
dista <- function(xnew,x,type = "euclidean",trans = TRUE,square = FALSE) {
	x <- .Call('Rfast_dista',t(xnew),t(x),square,type)
	if(trans){
		x <- t(x)
	}
	x
}