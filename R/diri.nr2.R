################################
#### Dirichlet distribution parameters
#### via Newton-Raphson
#### Tsagris Michail 8/2015
#### mtsagris@yahoo.gr
#### References: Estimating a Dirichlet distribution (2012)
#### Thomas P. Minka
#### http://research.microsoft.com/en-us/um/people/minka/papers/dirichlet/minka-dirichlet.pdf
################################

diri.nr2 <- function(x, tol = 1e-07) {
  ## x is compositional data
  
    runtime <- proc.time()
    x <- as.matrix(x)  ## makes sure x is a matrix
    x <- x/as.vector(rowsums(x))  ## makes sure x is compositional data
    n <- nrow(x)  ## sample size
    p <- ncol(x)
    zx <- t( log(x) )
    
    ma <- as.vector(rowmeans(zx))
    m <- as.vector(colmeans(x))
    down <-  - sum( m * ( ma - log(m) ) )
    sa <- 0.5 * (p - 1) / down  ## initial value for precision
    a1 <- sa * m  ## initial values
    
    f <- ma - digamma(a1) + digamma( sa )
    der <-  - trigamma(a1) + trigamma( sa )
    a2 <- a1 - f / der
    i <- 2
    
    a<-as.vector(diri_nr_type2(a1,a2,ma,tol))
    
    loglik <- n * lgamma( sum(a) ) - n * sum( lgamma(a) ) +
      sum( zx * (a - 1) )
    
    runtime <- proc.time() - runtime
    
  
  if ( is.null(colnames(x)) ) {
    names(a) <- paste("X", 1:p, sep = "")
  } else  names(a) <- colnames(x)
  
  list(loglik = loglik, param = a, runtime = runtime)
  
}