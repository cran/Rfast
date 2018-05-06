borel.mle <- function(x) {
  n <- length(x)
  sx <- sum(x) 
  m <- 1 - n / sx
  loglik <-  - sx + n + sum( (x - 1) * log(m * x) ) - sum( Rfast::Lgamma(x + 1) )
  list(loglik = loglik, m = m)
}