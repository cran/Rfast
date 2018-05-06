rm.lines <- function(y, x, logged = FALSE) {
  z <- x - mean( x )
  xi <- z / sum( z^2 )
  d <- length(x)
  n <- dim(y)[1]/d
  xi <- rep(xi, n)
  ina <- rep(1:n, each = d)
  be <- rowsum(xi * y, ina)
  s <- Rfast::colVars(be, std = TRUE)
  stat <- sqrt(n) * Rfast::colmeans(be) / s
  if ( logged ) {
    pvalue <- 2 * pt( abs(stat), n - 1, lower.tail = FALSE, log.p = TRUE ) 
  } else  pvalue <- 2 * pt( abs(stat), n - 1, lower.tail = FALSE) 
  cbind(stat, pvalue)
}