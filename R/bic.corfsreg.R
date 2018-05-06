bic.corfsreg <- function (y, x, tol = 2) {
    dm <- dim(x)
    n <- dm[1]
    p <- dm[2]
    con <- n * log(2 * pi) + n
    logn <- log(n)
    x <- Rfast::standardise(x, center = TRUE, scale = FALSE)
    y <- y - mean(y)
    model <- NULL
    tool <- sela <- numeric( min(n, p) )
    tool[1] <- n * log( Var(y) * (n - 1) /n) + 2 * logn 
    sela[1] <- 0
	options(warn = -1)
    yx <- cor(y, x)
    sel <- which.max(abs(yx))
    r <- yx[sel]
    z <- x[, sel, drop = FALSE]
    x[, sel] <- 0
    model <- .lm.fit(as.matrix(z), y)
    tool[2] <- n * log(sum(model$residuals^2)/n) + 3 * logn 
    sela[2] <- sel
    k <- 2         
    while ( k < n - 19 & tool[k - 1] - tool[k] > tol ) {
      m <- n - 3 - k
      k <- k + 1
      e1 <- .lm.fit(z, y)$residuals
      e2 <- .lm.fit(z, x)$residuals
      options(warn = -1)
      yx.z <- cor(e2, e1)
      sel <- which.max( abs(yx.z) )
      z <- cbind(z, x[, sel])
      x[, sel] <- 0
      model <- .lm.fit(z, y)
      tool[k] <- n * log( sum( model$residuals^2 )/n ) + (k + 1) * logn
      sela[k] <- sel
    }  
    res <- cbind(sela[1:c(k-1)], tool[1:c(k-1)] + con)
    colnames(res) <- c("sel", "bic")
    res
}
