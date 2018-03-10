
rows <- function(x,indices) {
	.Call('Rfast_rows', PACKAGE = 'Rfast',x,indices)
}