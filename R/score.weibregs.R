score.weibregs <- function(y, x, logged = FALSE) {
  mod <- Rfast::weibull.mle(y)
  k <- mod$param[1]     
  lam <- mod$param[2]
  yk <- y^k
  u <- k / lam^k * Rfast::colsums(yk * x) - k * Rfast::colsums(x)
  vu <- k^2 * Rfast::colsums(x^2)
  stat <- u^2 / vu
  pvalue <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
  cbind(stat, pvalue)
}