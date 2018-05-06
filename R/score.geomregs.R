score.geomregs <- function(y, x, logged = FALSE) {
  p <- Rfast::geom.mle(y)$prob
  sx <- Rfast::colsums(x)
  u <- (1 - p) * sx - p * Rfast::colsums( y * x)
  vb <- (1 - p ) * Rfast::colsums(x^2)
  stat <- u^2/vb
  pvalue <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
  cbind(stat, pvalue)
}