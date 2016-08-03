cora <- function(x) {
  ## x must be a matrix
  mat <- t( x )
  mat <- mat - as.vector(rowmeans(mat))
  mat <- mat / sqrt( as.vector(rowsums(mat^2)) )
  tcrossprod( mat )
}