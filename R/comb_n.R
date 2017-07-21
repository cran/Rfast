

comb_n <- function(n,k,trans = FALSE) {
	len<-length(n)
	if((len==1 && n<0) || k<0)
	  	stop("N and K must be positive numbers.")
	if(len==1){
	  x <- .Call('Rfast_k_comb_n', PACKAGE = 'Rfast',n,k)
	  dim(x)<-c(k,length(x)/k)
	  if(trans){
	  	return(t(x))
	  }
	  return(x)
	}
	.Call('Rfast_vec_comb_n', PACKAGE = 'Rfast',n,k)
}