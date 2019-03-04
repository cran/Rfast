

comb_n <- function(n,k,simplify=TRUE) {
	if(k<0){
	  	stop("K must be a positive number.")
	}
	if(length(n)==1) {
		n<-if(n<0) n:-1 else 1:n
	}
	.Call(Rfast_comb_n,n,k,simplify)
}