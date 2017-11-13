ct.mle <- function(x, tol = 1e-09) {

  n <- length(x)
  f <- 0.5 * n
  f2 <- 0.25 * n 
  x2 <- x^2
  s <- sum(x2)/n
  v <- 2 * s / abs(s - 1)
  y <- x2 / v
  lik1 <- n * lgamma(0.5 * v + 0.5) - n * 0.5 * log(v * pi) - n * lgamma(0.5 * v) - 
            0.5 * (v + 1) * sum( log1p(y) )
  ra <- 1 + y
  der <- f * digamma(0.5 * v + 0.5) - f / v - f * digamma(0.5 * v) -
       0.5 * sum( log(ra) ) + (v + 1) / 2 * sum( y / ra) / v
  der2 <- f2 * trigamma(0.5 * v + 0.5) + f / v^2 - f2 * trigamma(0.5 * v) + 
        sum( y / ra ) / v - 0.5 * (v + 1) * sum( ( 2 * y + y^2) / (v + x2 )^2 )   
  v <- v - der / der2
  y <- x2 / v
  lik2 <- n * lgamma(0.5 * v + 0.5) - n * 0.5 * log(v * pi) - n * lgamma(0.5 * v) - 
            0.5 * (v + 1) * sum( log1p(y) )
  i <- 2 

  while ( lik2 - lik1 > tol  &  v > 0) {
    i <- i + 1 
    lik1 <- lik2 
    ra <- 1 + y
    der <- f * digamma(0.5 * v + 0.5) - f / v - f * digamma(0.5 * v) -
          0.5 * sum( log(ra) ) + (v + 1) / 2 * sum( y / ra) / v
    der2 <- f2 * trigamma(0.5 * v + 0.5) + f / v^2 - f2 * trigamma(0.5 * v) + 
          sum( y / ra ) / v - 0.5 * (v + 1) * sum( ( 2 * y + y^2) / (v + x2 )^2 )   
    v <- v - der / der2
    y <- x2 / v
	lik2 <- n * lgamma(0.5 * v + 0.5) - n * 0.5 * log(v * pi) - n * lgamma(0.5 * v) - 
            0.5 * (v + 1) * sum( log1p(y) )
  }   
  list(iters = i, nu = v, loglik = lik2)
}  



