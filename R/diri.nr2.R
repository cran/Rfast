diri.nr2 <- function(x, type = 1, tol = 1e-07) {

  dm <- dim(x)
  n <- dm[1]  ## the sample size
  p <- dm[2]  ## dimensionality

  if (type == 1) {

    m <- colmeans(x)
    zx <- t( Log(x) )
    down <-  - sum( m * ( rowmeans( zx ) - log(m) ) )
    sa <- 0.5 * (p - 1) / down  ## initial value for precision
    a1 <- sa * m  ## initial values
    gm <- rowsums(zx)
    z <- n * Digamma( sa )
    g <- z - n * Digamma(a1) + gm
    qk <-  - n * Trigamma(a1)
    b <- sum(g / qk) / ( 1/z - sum(1 / qk) )
    a2 <- a1 - (g - b)/qk
    while ( sum( abs( a2 - a1 ) ) > tol ) {
      a1 <- a2
      z <- n * digamma( sum(a1) )
      g <- z - n * Digamma(a1) + gm
      qk <-  - n * Trigamma(a1)
      b <- sum(g / qk) / ( 1/z - sum(1 / qk) )
      a2 <- a1 - (g - b) / qk
    }
    loglik <- n * Lgamma( sum(a2) ) - n * sum( Lgamma(a2) ) + sum( zx * (a2 - 1) )
    if ( is.null(colnames(x)) ) {
      names(a2) <- paste("X", 1:p, sep = "")
    } else  names(a2) <- colnames(x)
    res <- list(loglik = loglik, param = a2)

  } else {
    zx <- t( Log(x) )
    ma <- rowmeans(zx)
    m <- colmeans(x)
    down <- -sum(m * (ma - log(m)))
    sa <- 0.5 * (p - 1)/down
    a1 <- sa * m
    f <- ma - Digamma(a1) + digamma(sa)
    der <-  - Trigamma(a1) + trigamma(sa)
    a2 <- a1 - f/der
    a <- .Call(Rfast_diri_nr_type2, a1, a2, ma, p, tol)
    loglik <- n * lgamma(sum(a)) - n * sum(Lgamma(a)) + sum(zx * (a - 1))
    if (is.null(colnames(x))) {
      names(a) <- paste("X", 1:p, sep = "")
    } else names(a) <- colnames(x)
    res <- list(loglik = loglik, param = a)
  }

  res
}
