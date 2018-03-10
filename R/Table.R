
Table <- function(x,y=NULL,names = TRUE,useNA = FALSE,rm.zeros = FALSE) {
	if(names){
		if(is.null(y)){
			.Call('Rfast_table_with_names', PACKAGE = 'Rfast',x)
		}else{
			x <- .Call('Rfast_table2_with_names', PACKAGE = 'Rfast',x,y,rm.zeros)
			f <- x$f
			rownames(f) <- x$x
			colnames(f) <- x$y
			f
		}
	}else if(is.null(y)){
		.Call('Rfast_table_c', PACKAGE = 'Rfast',x,useNA)
	}else{
		.Call('Rfast_table2_c', PACKAGE = 'Rfast',x,y,rm.zeros)
	}
}