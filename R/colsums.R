
colsums <- function(x) {
  	as.vector(.Call('Rfast_col_sums', PACKAGE = 'Rfast',x))
}