score.multinomregs <- function(y, x, logged = FALSE) {
  n <- length(y)
  p <- dim(x)[2]
  dof <- sort_unique.length(y) - 1
  if ( dof == 1 ) {
    res <- score.glms(y, x, oiko = "binomial", logged = logged) 
  } else {
    m0 <- numeric(dof)
    y1 <- design_matrix(y)[,-1]
    m <- colmeans(y1)   
    sx <- colsums(x)
    sx2 <- colsums(x^2)
    vp <- diag(m) - tcrossprod(m)  
    mx <- matrix( rep( m, rep(p, dof) ), ncol = dof )
    ni <- tabulate(y)
    u <- t( rowsum( x, y ) )[, -1] - sx * mx
    stat <- mahala(u, m0, vp ) / sx2
    pvalue <- pchisq( stat, dof, lower.tail = FALSE, log.p = logged )
    res <- cbind(stat, pvalue)
  } 
  res
}  
