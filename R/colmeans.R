
colmeans <- function(x) {
  	as.vector(.Call('Rfast_col_means', PACKAGE = 'Rfast',x))
}