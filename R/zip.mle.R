zip.mle <- function(x, tol = 1e-09) {
  no <- sum(x == 0)
  n <- length(x)  
  prop <- no / n
  n1 <- n - no
  x1 <- x[ x > 0 ]  
  sx <- sum(x1) 
  m <- sx / n
  s <- ( sum(x1^2) - m * sx ) / (n - 1)
  l1 <- s / m + m - 1
  fx <- m - m * exp(-l1) - l1 + prop * l1
  der <- m * exp(-l1) - 1 + prop
  l2 <- l1 - fx / der
  i <- 2
  while ( abs(l2 - l1 )> tol ) {
    i <- i + 1
    l1 <- l2
    fx <- m - m * exp(-l1) - l1 + prop * l1
    der <- m * exp(-l1) - 1 + prop
    l2 <- l1 - fx / der 
  }
  
  p <- 1 - m / l2
  loglik <- no * log( p + (1 - p) * exp(-l2) ) + n1 * log(1 - p) + sum( dpois(x1, l2, log = TRUE) )
  param <- c(l2, p)
  names(param) <- c("lambda", "pi")  
  list(iters = i, loglik = loglik, param = param)
}


