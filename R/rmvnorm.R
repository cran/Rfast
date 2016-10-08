################################
#### Random values generation from a multivariate normal distribution
#### Tsagris Michail 03/2014
#### mtsagris@yahoo.gr
################################

rmvnorm <- function(n, mu, sigma) {
  ## n is the sample size,
  ## mu is the mean vector and
  ## sigma is the covariance matrix
  ## sigma does not have to be of full rank

  p <- length(mu)
  x <- matrix( RcppZiggurat::zrnorm(n * p), ncol = p )  
  eig <- eigen(sigma)
  lam <- eig$values
  vec <- eig$vectors
  B <- vec %*% ( t(vec) * sqrt(lam) )
  x <- tcrossprod(B, x) +  mu
  t(x)

}
