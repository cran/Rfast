rayleigh.mle <- function(x) {
  n <- length(x)
  sigma <- 0.5 * sum(x^2) / n
  loglik <- sum( log(x) ) - n * log(sigma) - n
  list(loglik = loglik, sigma = sigma)
}