laplace.mle <- function(x) {
  n <- length(x)
  m <- Rfast::med(x)
  b <- sum( abs(x - m) ) / n
  param <- c(m, b)
  names(param) <- c("location", "scale")
  loglik <-  - n * log(2 * b) - n 
  list(loglik = loglik, param = param)
}