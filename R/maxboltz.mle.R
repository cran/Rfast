maxboltz.mle <- function(x) {
  n <- length(x)
  a <- sqrt( sum(x^2) / 3 / n )
  loglik <- n/2 * log(2 / pi) +  2 * sum( log(x) ) - 1.5 * n - 3 * n * log(a)
  list(loglik = loglik, a = a)
}
  