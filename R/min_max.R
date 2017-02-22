min_max<-function(x,index=FALSE){
	x <- .Call('Rfast_min_max', PACKAGE = 'Rfast',x,index)
	names(x) <- c("min","max")
	x
}