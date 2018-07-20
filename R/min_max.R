min_max<-function(x,index=FALSE,percent=FALSE){
	if(percent){
		x <- .Call(Rfast_min_max_perc,x)
		names(x) <- c("min","max","negative%","positive%")
		return(x)
	}
	x <- .Call(Rfast_min_max,x,index)
	names(x) <- c("min","max")
	x
}