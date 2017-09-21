wigner.mle <- function(x, tol = 1e-09) {
  n <- length(x)
  r <- max( abs(x ) )
  down <- r^2 - x^2
  list(loglik = n * log(2 / pi / r^2 ) + 0.5 * sum( log(down[down != 0]) ), R = r^2 )
}






