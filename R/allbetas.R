allbetas <- function(y, x, pvalue = FALSE, logged = FALSE) {

  if ( min(y) > 0 & max(y) < 1)  y <- log( y / (1 - y) )
  r <- as.vector( cov(y, x) )
  n <- length(y)
  my <- sum(y) / n
  m <- colmeans(x)
  sx <- colVars(x, suma = n * m)
  be <- r / sx
  a <- my - be * m

  if ( !pvalue )  {
    result <- cbind(a, be)
    if ( is.null( colnames(x) ) ) {
      rownames(result) <- paste("X", 1:ncol(x), sep = "" )
    } else    rownames(result) <- colnames(x)

  } else {
    sy <- Var(y)
    rho2 <- r^2 / (sx * sy)
    dof <- n - 2
    stat <- r * dof / (1 - rho2)
    pvalue <- pf( stat, 1, n - 2, lower.tail = FALSE, log.p = logged)
    result <- cbind(a, be, r, stat, pvalue)
    if ( is.null( colnames(x) ) ) {
      rownames(result) <- paste("X", 1:ncol(x), sep = "" )
    } else  rownames(result) <- colnames(x)
  }

  result
}