logseries.mle <- function(x, tol = 1e-09) {
  
  n <- length(x)
  sx <- sum(x)
  m <- sx / n
  p1 <- 1 / m
  p <- 1 - p1
  a1 <- log( p / p1 )
  loga1 <- log(p1)
  com <- p / loga1
  der <- m * p1 + com
  der2 <-  - m * p * p1 + p * p1 / loga1 + com^2
  a2 <- a1 - der / der2
  
  i <- 2
  while ( abs(a1 - a2) > tol ) {
    i <- i + 1
    a1 <- a2
    ea <- exp(a1)
    p <- ea / (1 + ea)
    p1 <- 1 - p
    loga1 <- log(p1)
    com <- p / loga1
    der <- m * p1 + com
    der2 <-  - m * p * p1 + p * p1 / loga1 + com^2
    a2 <- a1 - der / der2
  }
   
  list(iters = i, prob = p, loglik = sx * log(p) - sum( log(x) ) - n * log( -loga1 ) )
}






