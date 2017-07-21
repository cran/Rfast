pareto.mle <- function(x) {
  n <- length(x)
  xm <- min(x) 
  com <- n * log(xm)
  slx <- sum( log(x) )
  a <- n / ( slx - com )
  param <- c(xm, a)
  names(param) <- c("scale", "shape")
  loglik <- n * log(a) + a * com - (a + 1) * slx
  list(loglik = loglik, param = param)
}