chisq.mle <- function(x, tol = 1e-09) {

  n <- length(x)
  f1 <- 0.5 * n 
  f2 <- 0.5 * f1
  com <-  - f1 * log(2)
  slx2 <- 0.5 * sum( log(x) ) 
  sx <- sum(x)
  v1 <- sx / n
  der <-  com - f1 * digamma(0.5 * v1) + slx2
  der2 <-  - f2 * trigamma(0.5 * v1)
  v2 <- v1 - der / der2
  i <- 2
  while ( abs(v2 - v1) > tol ) {
    i <- i + 1 
    v1 <- v2
    der <-  com - f1 * digamma(0.5 * v1) + slx2
    der2 <-  - f2 * trigamma(0.5 * v1)
    v2 <- v1 - der / der2
  }

  list(iters = i , loglik = v2 * com - n * lgamma(0.5 * v2) + 2 * (0.5 * v2 - 1) * slx2 - 0.5 * sx, nu = v2)
}


