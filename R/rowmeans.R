
rowmeans <- function(x) {
  	as.vector(.Call(Rfast_row_means,x))
}