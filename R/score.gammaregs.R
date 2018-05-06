score.gammaregs <- function(y, x, logged = FALSE) {
  pa <- Rfast::gammamle(y)$param
  m <- pa[1]/pa[2]
  u <- Rfast::colsums(x) - Rfast::colsums(y * x)/m
  vb <- Rfast::colsums(x^2)/pa[1]
  stat <- u^2/vb
  pvalue <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
  cbind(stat, pvalue)
} 