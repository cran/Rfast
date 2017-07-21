score.ztpregs <- function(y, x, logged = FALSE) {  
  a <- ztp.mle(y)
  lam <- a$lambda
  elam <- exp(lam)
  u <- colsums(y * x) - lam * elam / (elam - 1) * colsums(x)
  ey <- lam * elam / (elam - 1) 
  vu <- colsums(x^2) * ( ey * ( 1 + lam - ey) )
  stat <- u^2/ vu
  pvalue <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
  cbind(stat, pvalue)
}
