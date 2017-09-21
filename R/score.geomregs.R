score.geomregs <- function(y, x, logged = FALSE) {
  p <- geom.mle(y)$prob
  sx <- colsums(x)
  u <- (1 - p) * sx - p * colsums( y * x)
  vb <- (1 - p ) * colsums(x^2)
  stat <- u^2/vb
  pvalue <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
  cbind(stat, pvalue)
}