poisson.cat1 <- function(y, x, logged = FALSE) {
  ni <- tabulate(x)
  si <- rowsum(y, x)
  mi <- si/ni
  n <- length(y)
  k <- length(ni)
  be <- log(mi)
  be <- c(be[1], be[-1] - be[1])
  s <- matrix(0, k, k)
  diag(s) <- si
  s[1, ] <- si
  s[, 1] <- si
  s[1, 1] <- sum(si)
  se <- sqrt( diag( chol2inv(chol(s)) ) )
  #########
  cl1 <- sum( si * log(mi) )
  cl0 <- s[1, 1] * log( s[1, 1]/n )  
  stat <- 2 * cl1 - 2 * cl0
  pvalue <- pchisq(stat, k - 1, lower.tail = FALSE, log.p = logged)
  res <- c(stat, pvalue)
  names(res) <- c("stat", "p-value")
  ########
  stat <- be^2/se^2
  pval <- pchisq(stat, 1, lower.tail = FALSE)
  mat <- cbind(be, se, stat, pval)
  colnames(mat) <- c("estimate", "std. error", "Wald", "p-value")
  list(info = mat, res = res)
}
 
