beta.mle <- function(x, tol = 1e-09) {
  n <- length(x)
  sly1 <- sum( log(x) ) / n
  sly2 <- sum( log(1 - x) ) / n
  sy <- sum(x)
  sy2 <- sum(x^2)
  iniphi <- (sy - sy2)/(sy2 - sy^2/n) * (n - 1)/n
  a <- sy * iniphi/n
  b <- iniphi - a
  phi <- a + b 
  lik1 <-  - n * lbeta(a, b) + (a - 1) * n * sly1 + (b - 1) * sly2 * n
  dera <- sly1 - digamma(a) + digamma(phi)
  derb <- sly2 - digamma(b) + digamma(phi)
  derab <- trigamma(phi)
  dera2 <-  - trigamma(a) + derab
  derb2 <-  - trigamma(b) + derab 
  anew <- c(a, b) - c(derb2 * dera - derab * derb, -derab * dera + dera2 * derb)/(dera2 * derb2 - derab^2)  
  a <- anew[1]     ;   b   <- anew[2]
  phi <- a + b
  lik2 <-  - n * lbeta(a, b) + (a - 1) * n * sly1 + (b - 1) * sly2 * n
  
  i <- 2
  while (lik2 - lik1 > tol) {
    i <- i + 1
	lik1 <- lik2
    dera <- sly1 - digamma(a) + digamma(phi)
    derb <- sly2 - digamma(b) + digamma(phi)
    derab <- trigamma(phi)
    dera2 <-  - trigamma(a) + derab
    derb2 <-  - trigamma(b) + derab 
    anew <- anew - c(derb2 * dera - derab * derb, -derab * dera + dera2 * derb)/(dera2 * derb2 - derab^2)  
	a <- anew[1]    ;   b <- anew[2]
    phi <- a + b
    lik2 <-  - n * lbeta(a, b) + (a - 1) * n * sly1 + (b - 1) * sly2 * n
  }
  loglik <-  lik2
  names(anew) <- c("alpha", "beta")
  list(iters = i, loglik = loglik, param = anew)
}

  







