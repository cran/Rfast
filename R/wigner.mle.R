wigner.mle <- function(x, tol = 1e-09) {

  n <- length(x)
  r1 <- 2 * Var(x, std = TRUE)
  x2 <- x^2
  down <- r1^2 - x2
  der <-  - 2 * n / r1 + 0.5 * sum(2 * r1 / down )
  der2 <- 2 * n / r1^2 - sum( (r1^2 + x2) /down^2 )
  r2 <- r1 - der / der2
  
  i <- i + 1 
  while ( abs(r1 - r2) > tol ) {
    i <- i + 1
    r1 <- r2 
    down <- r1^2 - x2
    der <-  - 2 * n / r1 + 0.5 * sum(2 * r1 / down )
    der2 <- 2 * n / r1^2 - sum( (r1^2 + x2) /down^2 )
    r2 <- r1 - der / der2
  }

  list(iters = i, loglik = n * log(2 / pi / r2^2 ) + 0.5 * sum( log(down) ), R = r2 )
}






