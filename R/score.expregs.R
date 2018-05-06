score.expregs <- function(y, x, logged = FALSE) {
  lam <- mean(y)
  u <- Rfast::colsums(x * y) * lam - Rfast::colsums(x) 
  vu <- Rfast::colsums(x^2) * lam^4
  stat <- u^2 / vu
  pvalue <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
  cbind(stat, pvalue)
}