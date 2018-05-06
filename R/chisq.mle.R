chisq.mle <- function(x, tol = 1e-09) {

  n <- length(x)
  f1 <- 0.5 * n 
  f2 <- 0.5 * f1
  com <-  - f1 * log(2)
  slx2 <- 0.5 * sum( log(x) ) 
  sx <- sum(x)
  v <- sx / n
  lik1 <- v * com - n * lgamma(0.5 * v) + 2 * (0.5 * v - 1) * slx2 
  der <-  com - f1 * digamma(0.5 * v) + slx2
  der2 <-  - f2 * trigamma(0.5 * v)
  v <- v - der / der2
  i <- 2
  lik2 <-  v * com - n * lgamma(0.5 * v) + 2 * (0.5 * v - 1) * slx2
  while ( lik2 - lik1 > tol ) {
    i <- i + 1 
	lik1 <- lik2
    der <-  com - f1 * digamma(0.5 * v) + slx2
    der2 <-  - f2 * trigamma(0.5 * v)
    v <- v - der / der2
    lik2 <-  v * com - n * lgamma(0.5 * v) + 2 * (0.5 * v - 1) * slx2
  }
  
  list(iters = i , loglik = lik2 - 0.5 * sx, nu = v)
}


