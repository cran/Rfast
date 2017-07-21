normlog.mle <- function(x) {
  n <- length(x)
  mx <- sum(x) /n
  m <- log( mx )
  sigma <- sum(x^2)/n - mx^2
  loglik <-  - n/2 * log(2 * pi * sigma) - n/2
  param <- c(mx, m, sigma, sigma * n / (n - 1) )
  names(param) <- c("exp_mu", "mu", "biased variance", "unbiased variance")
  list(loglik = loglik, param = param)
}