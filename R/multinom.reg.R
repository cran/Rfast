multinom.reg <- function(y, x, tol = 1e-07, maxiters = 50) {
  Y <- Rfast::design_matrix(y)[, -1]
  if ( !is.matrix(Y) ) {
    mod <- Rfast::glm_logistic(x, y, tol = tol, maxiters = maxiters)
    res <- list(iters = mod$iter, loglik = 0.5 * mod$devi, be = mod$be)
  } else {
    X <- model.matrix(y~., data.frame(x))
    p <- dim(X)[2] 
    dm <- dim(Y)
    n <- dm[1]
    d <- dm[2] 
    m <- Rfast::colmeans(Y)
    b0 <- Rfast::Log(m / (1 - m) )  
    b1 <- matrix( c(b0, numeric(p * d - d) ), nrow = p, ncol = d, byrow = TRUE)
    e <- Y - rep(m, rep(n, d) )
    id <- matrix(1:c(p * d), ncol = d)
    der <- numeric(d * p)
    der2 <- matrix(0, p * d, p * d)
    for (i in 1:d) { 
      der[id[, i]] <- Rfast::colsums( e[, i] * X )
      for (j in i:d) {
        if (i != j) {
          der2[id[, i], id[, j]] <- der2[id[, j], id[, i]] <-  - crossprod(m[i] * m[j] * X, X) 
        } else  der2[id[, i], id[, i]] <- crossprod(m[i] * (1 - m[i]) * X, X)
      }
   }  
    b2 <- b1 + solve(der2, der)
    k <- 2
    res <- try(while (sum(abs(b2 - b1)) > tol  &  k < maxiters) {
      k <- k + 1
      b1 <- b2
      m1 <- exp(X %*% b1) 
      m <- m1 / (rowsums(m1) + 1)
      e <- Y - m
      for (i in 1:d) { 
      der[id[, i]] <- Rfast::colsums( e[, i] * X )
        for (j in i:d) {
          if (i != j) {
            der2[id[, i], id[, j]] <- der2[id[, j], id[, i]] <-   - crossprod(m[, i] * m[, j] * X, X) 
          } else  der2[id[, i], id[, i]] <- crossprod(m[, i] * (1 - m[, i]) * X, X)
        }
      }  
      b2 <- b1 + solve(der2, der)
    }, silent = TRUE)
    if (class(res) == "try-error")   b2 <- b1
    colnames(b2) <- paste("Y", 1:d, sep = "")
    rownames(b2) <- colnames(X)
    Y1 <- design_matrix(y, ones = FALSE)
    m <- cbind(1, m1) 
    m <- m / rowsums(m)
    loglik <-  - sum(Y1 * log(Y1/m), na.rm = TRUE )
    res <- list(iters = k, loglik = loglik, be = b2)
  }
  res
}

