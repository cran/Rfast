colskewness <- function(x, pvalue = FALSE) {
  m <- Rfast::colmeans(x)
  y <- t(x) - m
  n <- dim(x)[1]
  nm1 <- n - 1
  up <- n * Rfast::rowsums(y^3)
  down <- ( Rfast::rowsums(y^2) / nm1 )^1.5
  skewness <- up / ( nm1 * (n - 2) * down )
  if (pvalue) {  
    vars <- 6 * n * nm1 / ( (n - 2) * (n + 1) * (n + 3) )
    stat <- skewness^2/vars
	pval <- pchisq(stat, 1, lower.tail = FALSE)
	skewness <- c(skewness, pval)
	names(skewness) <- c("skewness", "p-value")
  }
  skewness
}

