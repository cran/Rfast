cora <- function(x) {
  ## x must be a matrix
  mat <- t( x )
  mat <- mat - rowmeans(mat)
  mat <- mat / sqrt( rowsums(mat^2) )
  tcrossprod( mat )
}