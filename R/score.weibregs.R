score.weibregs <- function(y, x, logged = FALSE) {
  mod <- weibull.mle(y)
  k <- mod$param[1]      ;     lam <- mod$param[2]
  yk <- y^k
  eyk <- lam^(2 * k)
  u <-  - k * colsums(x) + k / lam^k * colsums(yk * x)
  vu <- k^2 * colsums(x^2)
  stat <- u^2 / vu
  pvalue <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
  cbind(stat, pvalue)
}