expregs <- function(y, x, di, tol = 1e-09, logged = FALSE) {
    
  dm <- dim(x)
  a0 <- sum(y) /dm[1] 
  di2 <- 1 - di
  sdi <-  - sum(di)
  lam <-  - sdi / sum(y)
  ini <-  - 2 * sdi * log(lam) + 2 * sdi
  con <- log( dm[1] )
  D <- dm[2]
  lik <- numeric(D)
  yyhat0 <- y * a0
  dera20 <-  - sum(yyhat0)  
  dera0 <- sdi - dera20
  
  for (j in 1:D) {
    X <- x[, j] 
    X2 <- X^2
    s2 <-  - sum(di * X)
    yyhat <- yyhat0
    dera2 <-  dera20  
    dera <- dera0
    derab <-  - sum(X * yyhat) 
    derb <-  s2 - derab
    derb2 <-  - sum(X2 * yyhat) 
    aold <- c(1/a0, 0)
    anew <- aold - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
  
    while ( sum( abs(anew - aold) ) > tol ) {
      aold <- anew
      a <- anew[1]     ;      b <- anew[2] 
      yyhat <- y * exp( -a - b * X)
      dera2 <-  - sum(yyhat)  
      dera <- sdi - dera2
      derab <-  - sum(X * yyhat) 
      derb <-  s2 - derab
      derb2 <-  - sum(X2 * yyhat) 
      anew <- aold - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 ) 
    }
    a <- anew[1]    ;     b <- anew[2]
    lik[j] <-  a * sdi + b * s2 + dera2
  }
    
  stat <-  2 * lik - ini
  pvalue <- pchisq(stat, 1, lower.tail = FALSE, log.p = logged)
  cbind(stat, pvalue)
}





## ela <- function(y, x, di) {
##  D <- ncol(x)
##  n <- length(di)
##  stat <- pval <- bic <- numeric(D)
##  z <- Surv(y, di)
##  for (i in 1:D) {
##    mod = survreg(z ~ x[, i], dist = "exponential")
##    stat[i] <- summary(mod)[13]$chi
##    pval[i] <- pchisq(stat[i], length(coef(mod))-1, lower.tail=FALSE)
##  }
##    cbind(stat, pval)
## }

