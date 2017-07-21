invgauss.mle <- function(x) {
  n <- length(x)
  sx <- sum(x)
  sx2 <- sum(1/x)
  m <- sx / n
  lambda <- 1 / ( sx2 / n - 1 / m )
  loglik <- n * 0.5 * log(lambda / 2 / pi) - 1.5 * sum( log(x) ) - lambda / 2 / m^2 * (-sx + m^2 * sx2)
  param <- c(m, lambda)
  names(param) <- c("mu", "lambda")
  list(loglik = loglik, param = param)
}