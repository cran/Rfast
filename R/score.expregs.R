score.expregs <- function(y, x, logged = FALSE) {
  lam <- mean(y)
  u <-  - colsums(x) + colsums(x * y) * lam
  vu <- colsums(x^2) * lam^4
  stat <- u^2 / vu
  pvalue <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
  cbind(stat, pvalue)
}