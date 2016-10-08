rmvlaplace <- function(n, lam, m, G) {
  ## n is the sample size
  ## lam is the parameter of the exponential distribution
  ## m is the mean vector
  ## G is a d x d covariance matrix with determinant 1

  if ( summary( det(G) )[1] == 1 ) {
    y <- paste("The determinant of the covariance matrix is not 1.")

    } else {
      d <- length(m)  ## dimensionality of the data
      z <- rexp(n, lam)
      x <- matrix( RcppZiggurat::zrnorm(n * d), ncol = d )
      eig <- eigen(G)
      val <- eig$values
      vec <- eig$vectors
      B <- vec %*% ( t(vec) * sqrt(lam) )  ## G^(0.5)
      y <- sqrt(z) * tcrossprod(x, B) + rep( m, rep(n, d) )  ## the simulated sample
   }
  
  y

}