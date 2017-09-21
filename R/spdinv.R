spdinv <- function(A) {
  chol2inv( cholesky(A) )
}