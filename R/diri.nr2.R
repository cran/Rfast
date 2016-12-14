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
  	dm <- dim(x)
    n <- dm[1]  ## sample size
    p <- dm[2]
    zx <- t( log(x) )
    
    ma <- rowmeans(zx)
    m <- colmeans(x)
    down <-  - sum( m * ( ma - log(m) ) )
    sa <- 0.5 * (p - 1) / down  ## initial value for precision
    a1 <- sa * m  ## initial values
    
    f <- ma - Digamma(a1) + digamma( sa )
    der <-  - Trigamma(a1) + trigamma( sa )
    a2 <- a1 - f / der
    
    a<-as.vector(.Call('Rfast_diri_nr_type2',a1,a2,ma,p,tol))
    
    loglik <- n * lgamma( sum(a) ) - n * sum( Lgamma(a) ) +
      sum( zx * (a - 1) )
       
  
  if ( is.null(colnames(x)) ) {
    names(a) <- paste("X", 1:p, sep = "")
  } else  names(a) <- colnames(x)
  
  list(loglik = loglik, param = a)
  
}