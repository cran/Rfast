score.betaregs <- function(y, x, logged = FALSE) {
  param <- Rfast::beta.mle(y)$param
  m1 <- digamma(param[1]) - digamma(param[2])
  z <- log(y) - log(1 - y)
  u <- Rfast::colsums( x * (z - m1) )  ## score function
  m2 <- trigamma(param[1]) + trigamma(param[2])
  seu <- Rfast::colsums( x^2) * m2  ## variance of the score
  stat <- u^2 / seu
  pvalue <- pchisq( stat, 1, lower.tail = FALSE, log.p = logged )
  cbind(stat, pvalue)
}


