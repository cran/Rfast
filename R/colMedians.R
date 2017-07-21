
colMedians <- function(x) {
	.Call('Rfast_col_meds', PACKAGE = 'Rfast',x)
}