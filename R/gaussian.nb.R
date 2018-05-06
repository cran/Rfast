gaussian.nb <- function(xnew = NULL, x, ina) {
  est <- NULL
  ni <- tabulate(ina)
  ni <- ni[ni > 0] 
  k <- length(ni)
  con <- 2 * log( ni )
  m <- rowsum(x, ina) / ni 
  s <- ( rowsum(x^2, ina) - m^2 * ni ) / (ni - 1)
  dets <- Rfast::rowsums( log(s) )
  if ( !is.null(xnew) ) {
    xnew <- t(xnew)
    mat <- matrix(nrow = dim(xnew)[2], ncol = k)
    for (j in 1:k)  mat[, j] <-  - Rfast::colsums( (xnew - m[j, ])^2 / s[j, ] ) - dets[j] + con[j]
    est <- Rfast::rowMaxs(mat)
  }
  rownames(m) <- rownames(s) <- paste("Group", 1:k)
  list(mu = m, sigma = s, ni = ni, est = est )
}
  