exp2.mle <- function(x) {
  a <- min(x)
  n <- length(x)
  b <- sum(x)/n - a
  param <- c(a, b)
  names(param) <- c("a", "b")
  loglik <-  - n * log(b) - 1/n
  list(loglik = loglik, param = param)
}  