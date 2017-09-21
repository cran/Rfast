ftests <- function(x, ina, logged = FALSE) {
  ni <- tabulate(ina)  ## sample sizes
  k <- length(ni)  ## number of groups
  m <- rowsum(x, ina) / ni
  s <- rowsum(x^2, ina)  
  s <- ( s - m^2 * ni ) / (ni - 1)
  w <- ni / s
  W <- colsums(w)
  mesi <- colsums(w * m) / W
  hi <- ( 1 - w/W )^2 / (ni - 1)
  H <- colsums(hi)
  f <- (k^2 - 1 ) / 3 / H 
  stat <- rowsums( ( t(w) * (t(m) - mesi)^2 ) / (k - 1)  / ( 1 + 2 * (k - 2)/(k^2 - 1) * H ) )
  pval <- pf(stat, k - 1, f, lower.tail = FALSE, log.p = logged)
  cbind(stat, pval)
} 