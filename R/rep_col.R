
rep_col <- function(x,n) {
	.Call('Rfast_rep_col', PACKAGE = 'Rfast',x,n)
}