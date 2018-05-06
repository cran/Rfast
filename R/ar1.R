ar1 <- function(y, method = "cmle") {
  N <- length(y)
  if ( method == "cmle" ) {
    dera2 <- N - 1
    derab <- sum( y[-N] )
    derb2 <- sum( y[-N]^2 )
    dera <- derab
    derb <- sum( y[-N] * y[-1])   
    cphi <- c( derb2 * dera - derab * derb, - derab * dera + dera2 * derb ) / ( dera2 * derb2 - derab^2 )
    s <- sum( (y[-1] - cphi[1] - cphi[2] * y[-N])^2 )/dera2
    param <- c(cphi, s)
    names(param) <- c("constant", "phi", "sigma")
  } else if ( method == "yw" ) {   
    m <- sum(y)/N
    z <- y - m
    phi <- sum( z[-N] * z[-1] ) / sum(z^2)
    sigma <- (1 - phi^2) * sum(z^2)/(N - 2)
    param <- c(m, phi, sigma)
    names(param) <- c("mean", "phi", "sigma")
  } else if ( method == "ols" ) {
    m <- sum(y)/N
    z <- y - m
    y1 <- z[-1] 
    x1 <- z[-N]
    phi <- as.vector( cov(y1, x1) )/ Var(x1)
    constant <-  mean(y1) - phi * mean(x1)
    sigma <- sum( (y1 - constant - phi * x1 )^2 ) / (N - 1)
    param <- c(constant, phi, sigma)
    names(param) <- c("constant", "phi", "sigma")
  }
  param 
}



