colexp2.mle <- function(x) {
  a <- Rfast::colMins(x)
  n <- dim(x)[1]
  b <- Rfast::colmeans(x)/n - a
  loglik <-  - n * log(b) - 1/n
  res <- cbind(a, b, loglik)
  colnames(res) <- c("a", "b", "log-likelihood")
  res
}  