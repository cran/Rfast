
colCumProds <- function(x) {
	.Call('Rfast_col_cum_prods', PACKAGE = 'Rfast',x)
}