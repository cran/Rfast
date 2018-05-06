invgauss.reg <- function(y, x, tol = 1e-08, maxiters = 100) {

  X <- model.matrix( ~ ., data.frame(x) )
  dm <- dim(X)
  ly <- log(y)
  y2 <- 2 * y  
  n <- dm[1]
  sy <- sum(1 / y)
  be1 <- solve( crossprod(X), crossprod(X, ly))
  mi <- exp( as.vector(X %*% be1) )
  f <- Rfast::colsums( (mi - y) / mi^2 * X )
  f2 <- crossprod( ( y2 / mi^2 - 1 / mi ) * X, X )
  be2 <- be1 - solve(f2, f)
  i <- 2
  while ( sum( abs(be1 - be2) ) > tol  |  i < maxiters) {
    i <- i + 1
    be1 <- be2
    mi <- as.vector( exp(X %*% be1) )
    f <- Rfast::colsums( (mi - y) / mi^2 * X )
    f2 <- crossprod( ( y2 / mi^2 - 1 / mi ) * X, X )
    be2 <- be1 - solve(f2, f)
  }
  lambda <- 1 / ( sy / n - sum( 1 / mi) / n )
  loglik <- n / 2 * log(lambda / 2 / pi) - 1.5 * sum( ly ) - 
            lambda / 2 * sum( (- y / mi^2 + sy / n) ) 
  deviance <- n/lambda
  phi <- sum( (y - mi)^2 / ( mi^3) )/(n - 2) 

  list(iters = i, loglik = loglik, deviance = deviance, 
       phi = phi, be = be2) 
}


