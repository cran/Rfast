rowcvs <- function(x, ln = FALSE, unbiased = FALSE) {
  if (ln) {
    s <- rowVars( Log(x) )
	cv <- sqrt( exp(s) - 1 )
  } else {
    m <- rowsums(x)
    n <- dim(x)[2]
    x2 <- rowsums(x^2)
    s <- (x2 - m^2/n)/(n - 1)
    cv <- n * sqrt(s) / m
  }	
  if (unbiased) cv <- (1 + 0.25 / n) * cv
  cv
}
