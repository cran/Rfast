logistic.cat1 <- function(y, x, logged = FALSE) {
  N <- Rfast::Table(y, x) 
  cj <- log( N[2, ] / N[1, ] )
  be <- c(cj[1], cj[-1] - cj[1])
  n <- length(y)
  Nj <- Rfast::colsums( N )
  pj <- N[2, ] / Nj
  p <- sum(N[2, ]) / n
  D0 <-  - 2 * n * sum( p * log(p) + (1 - p) * log(1 - p) )
  D1 <-  - 2 * sum( Nj * pj * log(pj) + Nj * (1 - pj) * log(1 - pj) )
  se <- pj * (1 - pj) * Nj 
  se <- c( 1/se[1], 1 / se[-1] + 1/se[1] )
  se <- sqrt(se)
  stat <- be/se
  pval <- pchisq(stat^2, 1, lower.tail = FALSE)
  mat <- cbind(be, se, stat^2, pval)
  colnames(mat) <- c("estimate", "std. error", "Wald", "p-value")
  devs <- c(D0, D1)
  stat <- D0 - D1
  pvalue <- pchisq(stat, length(be) - 1, lower.tail = FALSE, log.p = logged)
  devs <- c(D0, D1, stat, pvalue)
  names(devs) <- c("null deviance", "residual deviance", "stat", "p-value")
  list(info = mat, devs = devs)
}
  
