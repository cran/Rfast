score.negbinregs <- function(y, x, logged = FALSE) {
  mod <- Rfast::negbin.mle(y)
  r <- mod$param[2]    
  p <- mod$param[1]
  my <- mod$param[3]
  sxy <- Rfast::colsums(y * x)
  u <- sxy - (1 - p) * (sxy + r * Rfast::colsums(x) ) 
  vu <- Rfast::colsums(x^2) * p^2 * (my + my^2 / r)
  stat <- u^2 / vu
  pvalue <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
  cbind(stat, pvalue)
}