colpois.tests <- function(y, logged = FALSE) {
  m <- colmeans(y)
  n <- dim(y)[1]
  y2 <- colsums(y^2)
  s <- (y2 - n * m^2)/(n - 1)
  stat <- ( s/m - 1) * sqrt( 0.5 *(n - 1) )
  pval <- 2 * pnorm(abs(stat), lower.tail = FALSE, log.p = logged)
  cbind(stat, pval)
}