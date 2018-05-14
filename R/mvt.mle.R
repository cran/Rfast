mvt.mle <- function(x, v = 5, tol = 1e-07){
  ## x contains the data
  ## v is the degrees of freedom, set to 5 by default
  dm <- dim(x)
  p <- dm[2]   ;    n <- dm[1]  ## dimensions
  m <- Rfast::colmeans(x)  ## initial parameters
  y <- eachrow(x, m, oper = "-")
  R <- crossprod(y)/(n - 1)
  y <- NULL
  if (v != 1 ) R <- abs( v - 1 ) / v  * R     
  con <- n * lgamma( (v + p)/2 ) - n * lgamma(v/2) - 0.5 * n * p * log(pi * v)
  ### step 1
  wi <- (v + p) / ( v + Rfast::mahala(x, m, R) )  ## weights
  y <- sqrt(wi) * ( Rfast::eachrow(x, m, oper = "-" ) )
  sumwi <- sum(wi)
  R <- crossprod(y) / sumwi   ## scatter estimate
  m <- Rfast::colsums(wi * x) / sumwi  ## location estimate
  dis <- Rfast::mahala(x, m, R)
  el1 <-  - n * log( det(R) ) - (v + p) * sum( log1p(dis/v) ) 
  ### step 2
  wi <- (v + p) / ( v + dis )  ## weights
  y <- sqrt(wi) * (  Rfast::eachrow(x, m, oper = "-" ) ) 
  sumwi <- sum(wi)
  R <- crossprod(y) / sumwi  ## scatter estimate 
  m <- Rfast::colsums(wi * x) / sumwi  ## location estimate
  dis <- Rfast::mahala(x, m, R)
  el2 <-  - n * log( det(R) ) - (v + p) * sum( log1p(dis/v) ) 
  ## Step 3 and above
  i <- 2
  while ( el2 - el1 > tol ) { ## 1e-06 is the tolerance level 
    ## between two successive values of the log-likelihood
    i <- i + 1
    el1 <- el2
    wi <- (v + p) / ( v + dis) ## updated weights
    y <- sqrt(wi) * ( Rfast::eachrow(x, m, oper = "-" ) ) 
    sumwi <- sum(wi)
    R <- crossprod(y) / sumwi  ## updated scatter estimate
    m <- Rfast::colsums(wi * x) / sumwi  ## updated location estimate
    dis <- Rfast::mahala(x, m, R)
    el2 <-  - n * log( det(R) )- (v + p) * sum( log1p(dis/v) )  
  }  ## updated log-likelihood 

  list(iters = i, loglik = 0.5 * el2 + con, location = m, scatter = R) 
}