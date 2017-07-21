gumbel.mle <- function(x, tol = 1e-09) {

  n <- length(x) 
  m <- sum(x) / n
  x2 <- x^2
  s1 <- sqrt( 6 * sum(x2) / n - 6 * m^2 ) / pi
  y <- exp( - (x - m) / s1 )
  sy <- sum(y)
  co <- sum(x * y)
  f <- s1 - m + co / sy
  f2 <- 1 + ( sum(x2 * y) * sy - co^2 ) / s1^2 / sy^2
  s2 <- s1 - f / f2  
  i <- 2
  while ( abs(s1 - s2) > tol  )  {
    i <- i + 1
    s1 <- s2
    y <- exp( - (x - m) / s1 )
    sy <- sum(y)
    co <- sum(x * y)
    f <- s1 - m + co / sy
    f2 <- 1 + ( sum(x2 * y) * sy - co^2 ) / s1^2 / sy^2
    s2 <- s1 - f / f2
  } 
    
  y2 <- exp( - x / s2 )
  sy2 <- sum(y2)
  m <-  - s2 * log( sy2 / n )
  y <- (x - m) / s2
  lik <-  - n * log(s2) - sum(y) - sum( exp(-y) )
  param <- c(m, s2)
  names(param) <- c("location", "scale")
  list(iters = i, loglik = lik, param = param)
}








