rayleigh.mle <- function(x) {
 
  n <- length(x)
  sigma <- sum(x^2) / n / 2
  loglik <- sum( log(x) ) - n * log(sigma) - n
  
  list(loglik = loglik, sigma = sigma)

}