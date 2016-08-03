ftests <- function(x, ina) {

  ina <- as.numeric(ina)
  k <- max(ina)  ## number of groups
  ni <- as.vector( table(ina) )  ## sample sizes
  m <- s <- matrix( nrow = k, ncol = ncol(x) )

  for ( i in 1:k ) {
     y <- x[ina ==i, ]
     s[i, ] <- colVars( y )
     m[i, ] <- as.vector(colmeans(y))
  }
  
  w <- ni / s
  W <- as.vector(colsums(w))
  mesi <- as.vector(colsums(w * m)) / W
  hi <- ( 1 - w/W )^2 / (ni - 1)
  H <- as.vector(colSums(hi))
  f <- (k^2 - 1 ) / ( 3 * H )

  stat <- as.vector(rowsums( ( t(w) * (t(m) - mesi)^2 ) / (k - 1)  / ( 1 + 2 * (k - 2)/(k^2 - 1) * H ) ))
  pval <- pf(stat, k - 1, f, lower.tail = FALSE)

  cbind(stat, pval)

} 