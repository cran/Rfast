cova <- function(x) {
  ## x must be a matrix
  n <- nrow(x)  ## sample size
  mat <- t( x )
  mat <- mat - as.vector(rowmeans(mat))
  tcrossprod( mat ) / (n - 1)
}