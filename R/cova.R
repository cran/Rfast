cova <- function (x) {
  n <- dim(x)[1]
  m <- sqrt(n) * colmeans(x)
  (crossprod(x) - tcrossprod(m))/(n - 1)
}