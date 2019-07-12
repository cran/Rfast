#[export]
bic.corfsreg <- function (y, x, tol = 2) {
    dm <- dim(x)
    n <- dm[1]
    p <- dm[2]
    con <- n * log(2 * pi) + n
    logn <- log(n)
    x <- Rfast::standardise(x, center = TRUE, scale = TRUE)
    y <- ( y - mean(y) ) / Rfast::Var(y)
    model <- NULL
    tool <- sela <- numeric( min(n, p) )
    tool[1] <- n * log( Var(y) * (n - 1) /n) + 2 * logn 
    sela[1] <- 0
	oop <- options(warn = -1)
    on.exit( options(oop) )
    yx <- Rfast::eachcol.apply(x, y)
    sel <- which.max(abs(yx))
    r <- yx[sel] / (n - 1)
    z <- x[, sel, drop = FALSE]
    x[, sel] <- 0
    model <- .lm.fit(as.matrix(z), y)
    tool[2] <- n * log(sum(model$residuals^2)/n) + 3 * logn 
    sela[2] <- sel
    k <- 2         
    while ( k < n - 19 & tool[k - 1] - tool[k] > tol ) {
      m <- n - 3 - k
      k <- k + 1
      #e1 <- .lm.fit(z, y)$residuals
      #e2 <- .lm.fit(z, x)$residuals
      res <- .lm.fit(z, cbind(y, x))$residuals
      e1 <- res[, 1]
      e2 <- res[, -1]
      yx.z <- cor(e1, e2)
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
