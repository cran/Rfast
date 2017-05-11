### MLE of the projected normal, no gammas and identity covariance matrix
### Independent angular gaussian normal (rotational symmetry) using Newton - Raphson
iag.mle <- function(y, tol = 1e-07) {
  ## y is the spherical data, a matrix with unit vectors
  n <- dim(y)[1]  ## sample size 
  mod <- vmf.mle(y)
  ka <- mod$kappa
  ## step  1 
  y23 <- crossprod(y) - n * diag(3)
  m1 <- mod$mu * sqrt(ka)  
  a <- as.vector(y %*% m1)
  a2 <- a^2
  pa <- pnorm(a)
  da <- dnorm(a)
  gm <- pa + a2 * pa + a * da
  der <- 2 * (a * pa + da) * y
  sqa <- sqrt(pa) * y / sqrt(gm)
  sqa2 <- der / gm
  fm1 <- colsums(a * y) - n * m1 + colsums( sqa2 )
  fm2 <- y23 + 2 * crossprod(sqa) - crossprod(sqa2)
  m2 <- m1 - solve(fm2, fm1) 
  i <- 2

  while ( sum( abs(m2 - m1) ) > tol ) {
    m1 <- m2
    a <- as.vector(y %*% m1)
    a2 <- a^2
    pa <- pnorm(a)
    da <- dnorm(a)
    gm <- pa + a2 * pa + a * da
    der <- 2 * (a * pa + da) * y
    sqa2 <- der / gm
    fm1 <- colsums(a * y) - n * m1 + colsums( sqa2 ) 
    sqa <- sqrt(pa) * y / sqrt(gm)
    fm2 <- y23 + 2 * crossprod(sqa) - crossprod(sqa2)
    m2 <- m1 - solve(fm2, fm1) 
    i <- i + 1
  }
  
  rl <- sum(m2^2) 
  mesi <- rbind( m2, m2 / sqrt(rl) )
  rownames(mesi) <- c( "Mean vector", "Mean direction" ) 
  if ( is.null ( colnames(y) ) ) { 
    colnames(mesi) <- c("X", "Y", "Z")
  } else  colnames(mesi) <- colnames(y)  
  loglik <-  - n * log(2 * pi) + 0.5 * sum(a2) - n/2 * rl + sum( log(gm) )  
  l0 <-  - n * log( pi / 0.25 )
  param <- c(rl, loglik, l0)
  names(param) <- c( "Norm of mean", "Log likelihood", "Uniform log-likelihood")
  list(iters = i, mesi = mesi, param = param)
}