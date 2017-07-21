
rowrange <- function(x, cont = TRUE) {
  	if(cont){
		x <- .Call('Rfast_row_min_max', PACKAGE = 'Rfast',x)
		return(x[2,]-x[1,])
	}
	.Call('Rfast_row_len_sort_un_int', PACKAGE = 'Rfast',x)
}