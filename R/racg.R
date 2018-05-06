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
  x <- Rfast::matrnorm(n, p)
  x <- x %*% chol(sigma) 
  x / sqrt( Rfast::rowsums(x^2) )
}
