logcauchy.mle <- function(x, tol = 1e-09) {
  x <- log(x)
  a <- Rfast::cauchy.mle(x, tol = tol)$param
  m <- a[1]  ;  s <- a[2]
  loglik <- length(x) * log(s/pi) - sum(x) - sum( log( (x - m)^2 + s^2) ) 
  list(loglik = loglik, param = a)
}  
