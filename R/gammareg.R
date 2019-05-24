#[export]
gammareg <- function(y, x, tol = 1e-07, maxiters = 100) {
  X <- model.matrix( y~., data.frame(x) )
  sx <- Rfast::colsums(X)
  dm <- dim(X)
  p <- dm[2]
  n <- dm[1]
  mod <- Rfast::gammacon(y, tol = tol) 
  m <- exp(mod$be)
  be <- c( m, numeric(p - 1) )
  d1 <-  - 0.5 * mod$deviance - n
  con <- y * m
  com <- con * X
  der <-  - Rfast::colsums(com) + sx
  der2 <- crossprod(com, X)
  be <- be - solve(der2, der)
  m <- as.vector( exp( - X %*% be ) )
  con <- y * m
  d2 <- sum( log(con) - con )
  i <- 2
  while ( abs(d2 - d1) > tol  & i < maxiters) {
    i <- i + 1
    d1 <- d2
    com <- con * X
    der <-  - Rfast::colsums(com) + sx
    der2 <- crossprod(com, X)
    be <- be - solve(der2, der)
    m <- as.vector( exp( - X %*% be ) )
    con <- y * m
    d2 <- sum( log(con) - con )
  }
  phi <- sum( (con - 1)^2 ) / (n - p)
  devi <-  - 2 * d2 - 2 * n
  info <- c(i, devi, phi)
  names(info) <- c("iters", "deviance", "phi")
  names(be) <- colnames(X)
  list(info = info, be = be)
}