gammareg <- function(y, x, tol = 1e-08, maxiters = 100) {
  X <- model.matrix( y~., data.frame(x) )
  sx <- Rfast::colsums(X)
  ly <- log(y)
  mod <- lm.fit(X, ly )
  dm <- dim(X)
  p <- dm[2]
  n <- dm[1]
  b1 <- mod$coefficients
  m <- exp( - mod$fitted )
  com <- y * m * X
  der <-  - Rfast::colsums(com) + sx
  der2 <- crossprod(com, X)
  b2 <- b1 - solve(der2, der)
  i <- 2
  while ( sum( abs(b2 - b1) ) > tol  | i < maxiters) {
    i <- i + 1
    b1 <- b2
    m <- as.vector( exp( - X %*% b1 ) )
    com <- y * m * X
    der <-  - Rfast::colsums(com) + sx
    der2 <- crossprod(com, X)
    b2 <- b1 - solve(der2, der)
  }
  com <- y * m
  phi <- sum( (com - 1)^2 ) / (n - p)
  devi <-  - 2 * sum( log(com) - com ) - 2 * n
  info <- c(i, devi, phi)
  names(info) <- c("iters", "deviance", "phi")
  list(info = info, be = b2)
}

