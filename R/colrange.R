

colrange <- function(x, cont = TRUE){
	if(cont){
		x <- .Call(Rfast_col_min_max,x)
		return(x[2,]-x[1,])
	}
	.Call(Rfast_col_len_sort_un_int,x)
}