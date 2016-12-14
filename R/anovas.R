anovas <- function(x, ina, logged = FALSE) {
  
  k <- max(ina)
  ni <- tabulate(ina)
  n <- dim(x)[1]
  
  sx2 <- colsums(x^2) 
  m <- rowsum(x, ina)
  a <- colsums( m^2 / ni )
  b <- colsums( m )^2 / n 
  treat <- a - b
  error <- sx2 - a
  total <- sx2 - b
  fa <- ( treat / (k - 1) ) / ( error / (n - k) )
  
  pvalue <- pf(fa, k - 1, n - k, lower.tail = TRUE, log.p = logged)
  
  tab <- cbind(treat, error, total, fa, pvalue)
  colnames(tab) <- c( "SStreatment", "SSerror", "SStotal", "F value", "p-value" )
  if ( !is.null( colnames(x) ) )  rownames(tab) <- colnames(x)
  
  tab
  
}