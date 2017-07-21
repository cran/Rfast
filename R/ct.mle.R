ct.mle <- function(x, tol = 1e-09) {

  n <- length(x)
  f <- 0.5 * n
  f2 <- 0.25 * n 
  x2 <- x^2
  s <- sum(x2)/n
  v1 <- 2 * s / abs(s - 1)
  y <- x2 / v1
  ra <- 1 + y
  der <- f * digamma(0.5 * v1 + 0.5) - f / v1 - f * digamma(0.5 * v1) -
       0.5 * sum( log(ra) ) + (v1 + 1) / 2 * sum( y / ra) / v1
  der2 <- f2 * trigamma(0.5 * v1 + 0.5) + f / v1^2 - f2 * trigamma(0.5 * v1) + 
        sum( y / ra ) / v1 - 0.5 * (v1 + 1) * sum( ( 2 * y + y^2) / (v1 + x2 )^2 )   
  v2 <- v1 - der / der2
  i <- 2  

  while ( abs( v1 - v2) > tol  &  v2 > 0) {
    i <- i + 1 
    v1 <- v2
    y <- x2 / v1
    ra <- 1 + y
    der <- f * digamma(0.5 * v1 + 0.5) - f / v1 - f * digamma(0.5 * v1) -
          0.5 * sum( log(ra) ) + (v1 + 1) / 2 * sum( y / v1 / ra)
    der2 <- f2 * trigamma(0.5 * v1 + 0.5) + f / v1^2 - f2 * trigamma(0.5 * v1) + 
          sum( y / ra ) / v1 - 0.5 * (v1 + 1) * sum( ( 2 * y + y^2) / (v1 + x2 )^2 )   
    v2 <- v1 - der / der2
  }   

  loglik <- n * lgamma(0.5 * v1 + 0.5) - n * 0.5 * log(v1 * pi) - n * lgamma(0.5 * v1) - 
            0.5 * (v1 + 1) * sum( log1p(y) )
  list(iters = i, nu = v1, loglik = loglik)
}  



