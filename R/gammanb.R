gammanb <- function(xnew = NULL, x, ina, tol = 1e-07) {
  est <- NULL
  ni <- tabulate(ina)
  ni <- ni[ni > 0]
  k <- length(ni)
  d <- dim(x)[2]
  a <- matrix(0, k, d)
  b <- matrix(0, k, d)
  for (i in 1:k)  {
    res <- Rfast::colgammamle( x[ina==i, ] )[, 1:2]
    a[i, ] <- res[, 1]
    b[i, ] <- res[, 2]
  }
  rownames(a) <- rownames(b) <- paste("Group", 1:k)
  if ( is.null(xnew) ) {
    score <-  - tcrossprod(b, xnew) + tcrossprod(a - 1, Rfast::Log(xnew)) - 
    Rfast::rowsums( Rfast::Lgamma(a) - a * Rfast::Log(b) )
	est <- Rfast::colMaxs(score)
  }
  list( a = a, b = b, est = est )
}  