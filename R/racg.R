################################
#### Random values generation from the angular central Gaussian distribution
#### Tsagris Michail 03/2016
#### mtsagris@yahoo.gr
################################

racg <- function(n, sigma) {
  ## n is the sample size,
  ## mu is the mean vector and
  ## sigma is the covariance matrix
  ## sigma does not have to be of full rank

  p <- dim(sigma)[1]
  x <- matrix( RcppZiggurat::zrnorm(n * p), ncol = p )  
  eig <- eigen(sigma)
  lam <- eig$values
  vec <- eig$vectors
  B <- vec %*% ( t(vec) * sqrt(lam) )
  x <- tcrossprod(B, x) 
  x <- t(x)
  x / sqrt( rowsums(x^2) )

}
