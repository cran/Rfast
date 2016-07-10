correls <- function(y, x, type = "pearson", a = 0.05, rho = 0) {
  ## y is a numerical vector
  ## x is a matrix containing at least two variables
  ## type supported is either "pearson" or "spearman"
  ## a is the significance level
  ## rho is the hypothesised correlation

  n <- length(x)

  if (type == "pearson") {
    r <- as.vector( cor(y, x) ) ## the correlation value between y and all the xs
    zh0 <- 0.5 * log( (1 + rho) / (1 - rho) )  ## Fisher's transformation for Ho
    zh1 <- 0.5 * log( (1 + r) / (1 - r) )  ## Fisher's transformation for H1
    se <- 1/sqrt(n - 3)  ## standard error for Fisher's transformation of Ho

  } else if (type == "spearman") {
    r <- as.vector( cor(y, x, method = "spearman") ) ## the correlation value between y and all the xs
    zh0 <- 0.5 * log( (1 + rho) / (1 - rho) )  ## Fisher's transformation for Ho
    zh1 <- 0.5 * log( (1 + r) / (1 - r) )  ## Fisher's transformation for H1
    se <- 1.029563 / sqrt(n - 3)  ## standard error for Fisher's transformation of Ho
  }

  test <- as.vector( (zh1 - zh0) / se ) ## test statistic
  pvalue <-  2 * ( pt( -abs(test), n - 3, lower.tail = FALSE ) )  ## p-value
  b1 <- zh1 - qt(1 - a/2, n - 3) * se
  b2 <- zh1 + qt(1 - a/2, n - 3) * se
  ca <- cbind(b1 ,b2)
  ela <- exp( 2 * ca )
  ci <- ( ela - 1 ) / ( ela + 1 )  ## confidence intervals
  res <- cbind(r, pvalue, test, ci)
  colnames(res) <- c( 'correlation', 'p-value', 'z-stat',
                      paste( c( a/2 * 100, (1 - a/2) * 100 ), "%", sep = "") )

  if ( is.null(colnames(x)) ) {
    rownames(res) <- paste("X", 1:ncol(x), sep = "")
  } else  rownames(res) <- colnames(x)

  res
}
