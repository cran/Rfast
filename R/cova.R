cova <- function(x) {
  n <- dim(x)[1]
  m <- colmeans(x)
  ( crossprod(x) - n * tcrossprod(m) ) / (n - 1)
}
