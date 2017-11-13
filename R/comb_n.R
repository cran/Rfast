

comb_n <- function(n,k) {
	if(k<0){
	  	stop("K must be a positive number.")
	}
	if(length(n)==1 && (n%%2==1 || !(n%%2))){
		neg<-FALSE
		if(n<0){
			neg<-TRUE
			n<- -n
		}
	  	x <- .Call('Rfast_k_comb_n', PACKAGE = 'Rfast',n,k)
	  	if(neg){
	  		x<- -x
	  	}
	  	dim(x)<-c(k,length(x)/k)
	  	return(x)
	}
	.Call('Rfast_vec_comb_n', PACKAGE = 'Rfast',n,k)
}