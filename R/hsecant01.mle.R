hsecant01.mle <- function(x, tol = 1e-09) {
  
  sy1 <- sum( log(x) )  ;   sy2 <-  sum( log( 1 - x) )
  com <-  - 0.5 * sy1 - 0.5 * sy2
  comp <- sy1 / pi - sy2 / pi
  n <- length(x)
  a <-   - 0.5 * pi   ;   b <- 0.5 * pi
  ratio <- 2 / (sqrt(5) + 1)
  x1 <- b - ratio * (b - a)
  x2 <- a + ratio * (b - a)
  f1 <-  n * log( cos(x1) ) + x1 * comp
  f2 <-  n * log( cos(x2) ) + x2 * comp

  while (abs(b - a) > tol) {
    if (f2 < f1) {
      b <- x2
      x2 <- x1
      f2 <- f1
      x1 <- b - ratio * (b - a)
      f1 <- n * log( cos(x1) ) + x1 * comp
    } else {
      a <- x1
      x1 <- x2
      f1 <- f2
      x2 <- a + ratio * (b - a)
      f2 <- n * log( cos(x2) ) + x2 * comp
    }
  }
  theta <- (x1 + x2) / 2
  loglik <- n * log( cos(theta) / pi ) + com + theta * comp
  list(loglik = loglik, theta = theta)
}





#hsecant01 <- function(y, tol = 1e-09) {
#  sy1 <- sum( log(y) )  ;   sy2 <-  sum( log( 1 - y) )
#  com <-  - 0.5 * sy1 - 0.5 * sy2
#  comp <- sy1 / pi - sy2 / pi
#  n <- length(y)
#  f <-  function(theta, n, comp)  n * log( cos(theta)/pi ) + theta * comp
#  mod <- optimise(f, c(-pi/2, pi/2), n = n, comp = comp, maximum = TRUE, tol = tol)
#  loglik <- mod$objective + com
#  list(loglik = mod$objective + com, theta = mod$maximum)
#}
