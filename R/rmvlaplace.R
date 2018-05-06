rmvlaplace <- function(n, lam, mu, G) {
  ## n is the sample size
  ## lam is the parameter of the exponential distribution
  ## m is the mean vector
  ## G is a d x d covariance matrix with determinant 1
  if ( summary( det(G) )[1] == 1 ) {
    y <- paste("The determinant of the covariance matrix is not 1.")
  } else {
  	d <- length(mu)  ## dimensionality of the data
  	z <- rexp(n, lam)
  	x <- Rfast::matrnorm(n, d)
  	y <- sqrt(z) * x %*% chol(G) + rep(mu, rep(n, d) )## the simulated sample
  }
  y
}