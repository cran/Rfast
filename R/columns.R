
columns <- function(x,indices) {
	.Call('Rfast_columns', PACKAGE = 'Rfast',x,indices)
}