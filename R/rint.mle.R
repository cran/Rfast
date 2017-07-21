rint.mle <- function(x, ina, ranef = FALSE, tol = 1e-09) {
  n <- length(x)
  sxy <- sum(x)
  sx <- as.vector( group.sum(x, ina) )
  ni <- tabulate(ina)
  if ( Var(ni) == 0 ) {
    res <- varcomps.mle(x, ina, tol = tol)
  } else {
    mx <- sx / ni
    com <- ni * sx
    #############
    funa <- function(d, n, ni, S, hi2)  sum( log1p(ni * d) ) + n * log(S - d * sum(ni^2 * hi2/ (1 + ni * d) ) )    
    #############
    b1 <- sxy / n
    S <- sum( (x - b1)^2 )
    hi2 <- ( mx - b1 )^2
    mod <- optimise(funa, c(0, 50), n = n, ni = ni, S = S, hi2 = hi2, tol = tol)
    d <- mod$minimum
    down <- n - d * sum( ni^2/(1 + ni * d) ) 
    b2 <- ( sxy - d * sum( com/(1 + ni * d)) ) / down
    i <- 2
    ###########
    while ( sum( abs(b2 - b1) ) > tol ) {
      i <- i + 1
      b1 <- b2
      S <- sum( (x - b1)^2 )
      hi2 <- ( mx - b1 )^2
      mod <- optimise(funa, c(0, 50), n = n, ni = ni, S = S, hi2 = hi2, tol = tol)
      d <- mod$minimum 
      down <- n - d * sum( ni^2/(1 + ni * d) ) 
      b2 <- ( sxy - d * sum( com/(1 + ni * d)) ) / down
    }
    sigma <- S / n
    se <- sigma/(1 + d )
    tau <- sigma - se
    loglik <-  - 0.5 * mod$objective - 0.5 * n * (log(2 * pi) - log(n) + 1) 
    info <- c(i, tau, se, loglik )
    names(info) <- c("iters", "sigma_tau", "sigma_errors", "log-lik")
    res <- list(info = info, my = b2)
    if (ranef) {
      ranef <- ( mx - b2 ) * d * ni/(1 + ni * d)
      res <- list(info = info, my = b2, ranef = ranef)
    }
    res
  }   
  res
}
