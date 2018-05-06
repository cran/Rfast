colnormal.mle <- function(x) {
  n <- dim(x)[1]
  m <- Rfast::colmeans(x)
  s <- (Rfast::colsums(x^2) - n * m^2)/(n - 1)
  loglik <-  - 0.5 * n * ( log(2 * pi) + log(s) ) - 0.5 * (n - 1)
  res <- cbind(m, s, loglik)
  colnames(res) <- c("mean", "unbiased variance", "log-likelihood")
  res
}
