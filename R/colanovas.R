colanovas <- function(y, x, logged = FALSE) {
  if ( is.data.frame(x) ) {
	x <- Rfast::data.frame.to_matrix(x)
  }
  n <- dim(x)[1]
  b <- sum(y)^2/n
  sy2 <- sum(y^2)
  a <- .Call('Rfast_col_anovas', PACKAGE = 'Rfast',y,x)
  k <- Rfast::colrange(x,cont=FALSE)
  mst <- (a - b) / (k - 1)
  mse <- (sy2 - a) / (n - k)
  fa <- mst / mse
  pvalue <- pf(fa, k - 1, n - k, lower.tail = FALSE, log.p = logged)
  tab <- cbind(fa, pvalue)
  colnames(tab) <- c("F stat", "p-value")
  tab
}