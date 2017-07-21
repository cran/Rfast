
rep_col <- function(x,ncols) {
	.Call('Rfast_rep_col', PACKAGE = 'Rfast',x,ncols)
}