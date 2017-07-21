halfnorm.mle <- function(x) {
  n <- length(x)
  s <- sqrt( sum(x^2) / n )
  loglik <- n / 2 * log(2 / s / pi) - n / 2
  list(loglik = loglik, sigma.squared = s)
}
  