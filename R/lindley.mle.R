lindley.mle <- function(x) {
  n <- length(x)
  sx <- sum(x)
  a <- sx/n
  b <- a - 1
  delta <- b^2 + 8 * a
  theta <-  0.5 * ( - b + sqrt(delta) ) / a
  loglik <- 2 * n * log(theta) - n * log(1 + theta) + sum( log1p(x) ) - theta * sx
  list(loglik = loglik, theta = theta)
}
