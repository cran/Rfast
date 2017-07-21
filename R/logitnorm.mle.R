logitnorm.mle <- function(x) {
  n <- length(x)
  lx1 <- log(x)
  lx2 <- log(1 - x)
  y <- lx1 - lx2 
  sy <- sum(y)
  m <- sy / n
  s <- ( sum(y^2) - n * m^2 ) / n
  loglik <- sum( dnorm(y, m, sqrt(s), log = TRUE ) ) - sy
  param <- c(m, n * s / (n - 1) )
  names(param) <- c("mean", "unbiased variance")
  list(loglik = loglik, param = param)
}