score.invgaussregs <- function(y, x, logged = FALSE) {
  n <- length(y)
  m <- sum(y) / n
  lambda <- 1 / ( sum(1/y) / n - 1 / m )
  u <- Rfast::colsums( (m - y) * x ) * lambda
  vu <- m^3 * Rfast::colsums(x^2)
  stat <- u^2 / vu
  pvalue <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
  cbind(stat, pvalue)
}
