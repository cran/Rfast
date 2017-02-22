weibull.mle <- function(x, tol = 1e-09) {

  n <- length(x)
  lx <- log(x)
  lx2 <- lx^2
  mlx <- sum(lx) / n
  b1 <- 1
  y <- x^b1  
  co <- sum(y * lx)
  sy <- sum(y)
  fb <- 1 / b1 + mlx - co / sy 
  fb2 <-  - 1 / b1^2  - ( sum(y * lx2) * sy - co^2 ) / sy^2
  b2 <- b1 - fb / fb2
  i <- 2  
  while ( sum( abs(b2 - b1) ) > tol ) {
    b1 <- b2
    i <- i + 1
    y <- x^b1  
    co <- sum(y * lx)
    sy <- sum(y)
    fb <- 1 / b1 + mlx - co / sy 
    fb2 <-  - 1 / b1^2  - ( sum(y * lx2) * sy - co^2 ) / sy^2
    b2 <- b1 - fb / fb2
  }
  theta <- ( sy / n )^( 1 / b2 )
  lik <- n * log(b2) - n * b2 * log(theta) + (b2 - 1) * n * mlx - sum( (x / theta)^b2 )
  param <- c(b2, theta)
  names(param) <- c("shape", "scale" )
  list(iters = i, loglik = lik, param = param)
}











