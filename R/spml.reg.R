spml.reg <- function(y, x, tol = 1e-07, seb = FALSE) {
  ## y is the angular dependent variable
  ## x contains the independent variable(s)
  x <- model.matrix(~., data.frame(x) )
  if ( is.matrix(y) )  {
    u <- y
	ci <- u[, 1]
	si <- u[, 2]
  } else {
    ci <- cos(y)  ;   si <- sin(y)
    u <- cbind(ci, si)  ## bring the data onto the circle
  }
  n <- dim(u)[1]
  XX <- solve( crossprod(x), t(x) )
  B1 <-  XX %*% u 
  mu <- x %*% B1 
  f <-  - 0.5   ;   con <- sqrt(2 * pi) 
  tau <- Rfast::rowsums(u * mu)
  ptau <- pnorm(tau)
  psit <- tau + ptau / ( exp(f * tau^2)/con + tau * ptau )
  B2 <- XX %*% (psit * u)
  mu <- mu * psit        
  tau <- Rfast::rowsums(u * mu)
  ptau <- pnorm(tau)
  ## mono th while
  l <- .Call('Rfast_spml_reg_helper', PACKAGE = 'Rfast',B1,B2,x,u,ci,si,con,tol)
  B2 <- l$B2
  der2 <- l$der2
  tau <- l$tau
  ptau <- l$ptau
  mu <- l$mu
  ###
  if ( seb ) {
    seb <-  sqrt( diag( solve( -der2 ) ) )
    seb <- matrix(seb, ncol = 2)
    colnames(seb) <- c("Cosinus of y", "Sinus of y")
    rownames(seb) <- colnames(x)    
  } else   seb <- NULL
  loglik <-  - 0.5 * sum( mu^2 ) + sum( log1p( tau * ptau * con / exp(f * tau^2) ) ) - n * log(2 * pi)
  colnames(B2) <- c("Cosinus of y", "Sinus of y")
  rownames(B2) <- colnames(x)
  list(iters = l$i, loglik = loglik, be = B2, seb = seb)  
}