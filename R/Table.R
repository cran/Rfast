
Table <- function(x,as.vector = TRUE) {
	if(is.character(x)){
		x <- .Call('Rfast_table_string', PACKAGE = 'Rfast',x)
	}else{
		x <- .Call('Rfast_table_double', PACKAGE = 'Rfast',x)
	}
	if(as.vector){
		y<-x$freqs
		names(y)<-x$values
		y
	}else x
}