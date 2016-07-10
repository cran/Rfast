cora <- function(x) {
  ## x must be a matrix
  mat <- t( x )
  mat <- mat - rowMeans(mat)
  mat <- mat / sqrt( rowSums(mat^2) )
  tcrossprod( mat )
}
