################################
#### Random values generation from a multivariate t distribution
#### Tsagris Michail 07/2014
#### mtsagris@yahoo.gr
################################
rmvt <- function(n, mu, sigma, v) {
  ## n is the sample size
  ## mu is the mean vector
  ## sigma is the covariance matrix
  ## sigma does not have to be of full rank
  ## v is the degrees of freedom
  p <- length(mu)
  x <- Rfast::matrnorm(n, p)
  w <- sqrt( v / rchisq(n, v) )
  w * x %*% chol(sigma) + rep(mu, rep(n, p) )
}
