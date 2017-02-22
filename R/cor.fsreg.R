cor.fsreg <- function(y, x, threshold = 0.05, tol = 2) {
  threshold <- log(threshold)
  dm <- dim(x)
  n <- dm[1]
  p <- dm[2]
  con <- n * log(2 * pi) + n  
  logn <- log(n)
  x <- standardise(x, center = TRUE, scale = FALSE)
  y <- y - mean(y)
  tool <- numeric( min(n, p) )
  info <- cbind(0, 0, 0)
  yx <- cor(y, x) 
  sel <- which.max( abs(yx) )
  r <- yx[sel]
  stat <- abs( 0.5 * log( (1 + r) / (1 - r) ) * sqrt(n - 3) )  ## 
  pv <- log(2) + pt(stat, n - 3, lower.tail = FALSE, log.p = TRUE)  ## logged p-values  
  if ( pv < threshold ) {
    info <- cbind(sel, pv, stat)
    z <- x[, sel]
    model <- .lm.fit(as.matrix(z), y)
    tool[1] <- n * log( mean(model$residuals^2) ) + con + 3 * logn 
  } else {
    info <- c(Inf, 0, 0) 
  }  
  
  if ( !is.null(model) ) {
    xz <- as.vector( cor(z, x) )
    yx.z <- ( yx - xz * r ) / sqrt(1 - xz^2) / sqrt(1 - r^2)
    sel <- which.max( abs(yx.z) )
    r <- yx.z[sel]
    stat <- 0.5 * abs( log( (1 + r) / (1 - r) ) ) * sqrt(n - 4)    
    pv <- log(2) + pt(stat, n - 4, lower.tail = FALSE, log.p = TRUE)  ## logged p-values  
    if ( pv < threshold )  {
      model <- .lm.fit( cbind(z, x[, sel]), y )
      tool[2] <-   n * log( mean(model$residuals^2) ) + con + 4 * logn
      if ( tool[1] - tool[2] > tol ) {
        info <- rbind(info, c(sel, pv, stat) )
      } else  info <- info 
    } else  info <- info 

  }
  k <- 2
  
  if ( nrow(info) > 1 ) { 
    while ( info[k, 2] < threshold  &  k < n   &  tool[ k - 1 ] - tool[ k ] > tol  &  k < p )  {
      sela <- info[, 1]
      m <- n - 3 - k 
      k <- k + 1
      z <- x[, sela]
      e1 <- .lm.fit(z, y)$residuals
      e2 <- .lm.fit(z, x)$residuals
      ## yx.z <- colsums(e1 * e2) / sqrt( colsums(e2^2) * sum(e1^2) ) 
      yx.z <- cor(e2, e1) 
      sel <- which.max( abs(yx.z) )  
      r <- yx.z[sel]
      stat <- abs( 0.5 * log( (1 + r) / (1 - r) ) * sqrt(m) )  ## 
      pv <- log(2) + pt(stat, m, lower.tail = FALSE, log.p = TRUE)  ## logged p-values  
      if ( pv < threshold )  {
        model <- .lm.fit( cbind(z, x[, sel]), y )
        tool[k] <- n * log( mean(model$residuals^2) ) + con + (k + 2) * logn
        if ( tool[k - 1] - tool[k] > tol ) {
          info <- rbind(info, c(sel, pv, stat) )
        } else  info <- rbind(info, c(Inf, 0, 0)) 
      } else  info <- rbind(info, c(Inf, 0, 0)) 

    } 
  }
  tool <- tool[ 1:c(k-1) ] 
  info <- info[1:c(k - 1), ]
  if( !is.matrix(info) )  { 
    info <- c(info, tool)
    names(info)[4] <- "bic"
  } else {
    info <- cbind(info, tool)
    colnames(info)[4] = "bic"
  } 
  info
}
