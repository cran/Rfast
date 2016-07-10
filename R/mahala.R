mahala <- function(x, m, s, inverted = FALSE) {

  ## x must be a matrix
  ## m is the mean vector and
  ## s is the covariance matrix
  ## if s is the inverse of the covariance matrix
  ## put inverted = TRUE

  y <- t(x) - m

  if ( inverted == FALSE ) {
    sa <- solve(s)

  } else {
    sa <- s
  }

  colSums( y * crossprod(sa, y) )

}
