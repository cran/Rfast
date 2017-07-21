binom.mle <- function(x, N = NULL, tol = 1e-07) {
  
  if ( !is.null(N) ) {
    p <- sum(x) / length(x) / N 
    loglik <- sum( dbinom(x, N, p, log = TRUE) )
    res <- list(loglik = loglik, prob = p)
  } else {
   
    n <- length(x)
    k <- max(x)
    sx <- sum(x)
    p <- sx / n / k
    x1 <- x - 1 
    y <- k - x1
    derk <- n * digamma(k + 1) - sum( digamma(y) ) + n * log(1 - p)
    derp <- sx / p + (sx - n * k) / (1 - p)
    derk2 <- n * trigamma(k + 1) - sum( trigamma(y) )
    derp2 <-  - sx / p^2 + (sx - n * k) / (1 - p)^2
    derkp <-  - n / (1 - p)
    aold <- c(k, p)
    anew <- aold - c( derp2 * derk - derkp * derp, - derkp * derk + derk2 * derp ) / ( derk2 * derp2 - derkp^2 )
     
    i <- 2
    while ( sum( abs(aold - anew) ) > tol ) {
      i <- i + 1
      aold <- anew
      k <- aold[1]  ;  p <- aold[2]
      y <- k - x1
      derk <- n * digamma(k + 1) - sum( digamma(y) ) + n * log(1 - p)
      derp <- sx / p + (sx - n * k) / (1 - p)
      derk2 <- n * trigamma(k + 1) - sum( trigamma(y) )
      derp2 <-  - sx / p^2 + (sx - n * k) / (1 - p)^2
      derkp <-  - n / (1 - p)
      anew <- aold - c( derp2 * derk - derkp * derp, - derkp * derk + derk2 * derp ) / ( derk2 * derp2 - derkp^2 )    
    }
    k <- round( anew[1] )   ;   p <- anew[2]
    loglik <- sum( dbinom(x, k, p, log = TRUE) )
    names(anew) <- c("Number of trials", "probability")
    res <- list(iters = i, loglik = loglik, param = anew)
  }   
  
  res
}
  
  