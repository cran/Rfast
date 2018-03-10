
countNA <- function(x) {
	.Call('Rfast_count_na', PACKAGE = 'Rfast',x)
}