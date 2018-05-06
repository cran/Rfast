
colCumMins <- function(x) {
	.Call('Rfast_col_cum_mins', PACKAGE = 'Rfast',x)
}