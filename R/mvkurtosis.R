mvkurtosis <- function(x) {
  n <- dim(x)[1]
  m <- colmeans(x)
  s <- (crossprod(x) - n * tcrossprod(m))/(n - 1)
  sum( mahala(x, m, s)^2 ) / n 
}