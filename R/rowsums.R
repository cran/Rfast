
rowsums <- function(x) {
  	as.vector(.Call('Rfast_row_sums', PACKAGE = 'Rfast',x))
}