
rep_row <- function(x,n) {
	.Call('Rfast_rep_row', PACKAGE = 'Rfast',x,n)
}