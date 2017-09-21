corpairs <- function(x, y, rho = NULL, logged = FALSE) {

  mx <- colsums(x)
  my <- colsums(y)
  mx2 <- colsums(x^2)
  my2 <- colsums(y^2)
  n <- dim(x)[1]
  up <- n * colsums(x * y) - mx * my
  down <- (n * mx2 - mx^2) * (n * my2 - my^2)
  r <- up / sqrt( down ) 
  
  if ( !is.null(rho) ) {
    zh0 <- 0.5 * log( (1 + rho)/(1 - rho) )
    zh1 <- 0.5 * log( (1 + r)/(1 - r) )
    se <- 1 / sqrt(n - 3)
    test <- as.vector( (zh1 - zh0) / se )
    if ( logged ) { 
       pvalue <- log(2) + pt( abs(test), n - 3, lower.tail = FALSE, log.p = TRUE)
    } else  pvalue <- 2 * pt( abs(test), n - 3, lower.tail = FALSE )
    r <- cbind(r, test, pvalue)
    colnames(r) <- c("correlation", "z-stat", "p-value") 
  }
  
  r  
}