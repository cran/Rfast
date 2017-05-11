
dista <- function(xnew,x,type = "euclidean",trans=TRUE) {
	if(trans){
		return(t(.Call('Rfast_dista',t(xnew),t(x),type)	))
	}
	.Call('Rfast_dista',t(xnew),t(x),type)
}