beta.mle <- function(x, tol = 1e-09) {
         
  n <- length(x)
  sly1 <- sum( log(x) )      ;      sly2 <- sum( log(1 - x) )  
  sy <- sum(x)      ;   sy2  <- sum(x^2)
  iniphi <- ( sy - sy2 ) / ( sy2 - sy^2 / n ) * (n - 1) / n
  a <- sum(x) * iniphi / n        ;        b <- iniphi - a
  
  phi <- a + b
  derab <- n * trigamma(phi)
  dera <- n * digamma(phi) - n * digamma(a) + sly1
  dera2 <-  n * trigamma(phi) - n * trigamma(a)
  derb <-  n * digamma(phi) - n * digamma(b) + sly2
  derb2 <- n * trigamma(phi) - n *trigamma(b)
  
  aold <- c(a, b)
  anew <- aold - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
  
  i <- 2
  while ( sum( abs(anew - aold) ) > tol ) {
    i <- i + 1
    aold <- anew
    a <- anew[1]     ;      b <- anew[2] 

    phi <- a + b
    derab <- n * trigamma(phi)
    dera <- n * digamma(phi) - n * digamma(a) + sly1
    dera2 <-  n * trigamma(phi) - n * trigamma(a)
    derb <-  n * digamma(phi) - n * digamma(b) + sly2
    derb2 <- n * trigamma(phi) - n *trigamma(b)
  
    anew <- aold - c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
  }

  a <- anew[1]    ;     b <- anew[2]
  
  loglik <-  - n * lbeta(a, b) + (a - 1) * sly1 + (b - 1) * sly2
  
  names(anew) <- c("alpha", "beta")
  list(iters = i, loglik = loglik, param = anew) 

}
    