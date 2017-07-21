#### Projected multivariate normal MLE
#### Presnell, Morrison and Littell (1998), JASA
################################
spml.mle <- function(x, tol = 1e-09) {
  ci <- cos(x)  ;   si <- sin(x)
  u <- cbind(ci, si)  ## bring the data onto the circle
  su <- colsums(u)
  n <- dim(u)[1]
  ini <- vmf.mle(u)
  mu1 <- ini$mu * ini$kappa
  f <-  - 0.5   ;   con <- sqrt(2 * pi) 
  tau <-  as.vector( u %*% mu1 )
  ptau <- pnorm(tau)
  rat <- ptau / ( exp(f * tau^2)/con + tau * ptau )
  psit <- tau + rat    
  psit2 <- 2 - tau * rat - rat^2
  der <- colsums(psit * u) - n * mu1 
  dera <- der[1]   ;  derb <- der[2]
  dera2 <- sum( psit2 * ci^2 - 1) 
  derab <- sum( psit2 * ci * si ) 
  derb2 <- sum( psit2 * si^2 - 1 ) 
  mu2 <- mu1 - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
 
  i <- 2
  while ( sum( abs(mu2 - mu1) ) > tol ) {
    i <- i + 1
    mu1 <- mu2
    tau <-  as.vector( u %*% mu1 )
    ptau <- pnorm(tau)
    rat <- ptau / ( exp(f * tau^2)/con + tau * ptau )
    psit <- tau + rat    
    psit2 <- 2 - tau * rat - rat^2
    der <- colsums(psit * u) - n * mu1 
    dera <- der[1]   ;  derb <- der[2]
    dera2 <- sum( psit2 * ci^2 - 1) 
    derab <- sum( psit2 * ci * si ) 
    derb2 <- sum( psit2 * si^2 - 1 ) 
    mu2 <- mu1 - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
  }

  gam <- sum(mu2^2)
  loglik <-  - 0.5 * n * gam + sum( log1p( tau * ptau * con / exp(f * tau^2) ) ) - n * log(2 * pi)
  list(iters = i, loglik = loglik, gamma = gam, mu = mu2)  
}


