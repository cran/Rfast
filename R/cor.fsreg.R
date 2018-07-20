cor.fsreg <- function(y, x, threshold = 0.05, tolb = 2, tolr = 0.02, stopping = "BIC") {
  threshold <- log(threshold)
  dm <- dim(x)
  n <- dm[1]
  p <- dm[2]
  con <- n * log(2 * pi) + n  
  logn <- log(n)
  x <- Rfast::standardise(x, center = TRUE, scale = FALSE)
  y <- y - mean(y)
  options(warn = -1)
  yx <- cor(y, x) 
  sel <- which.max( abs(yx) )
  r <- yx[sel]
  stat <- abs( 0.5 * log( (1 + r) / (1 - r) ) * sqrt(n - 3) )  ## 
  pv <- log(2) + pt(stat, n - 3, lower.tail = FALSE, log.p = TRUE)  ## logged p-values  
  model <- NULL
  #############################
  ###### BIC stopping criterion
  #############################
  if ( stopping == "BIC" ) {
    info <- cbind(0, 0, 0)
    tool <- numeric( min(n, p) ) 
    if ( pv < threshold ) {
      info <- cbind(sel, pv, stat)
      z <- x[, sel, drop = FALSE]
      model <- .lm.fit(z, y)
      tool[1] <- n * log( sum(model$residuals^2)/n ) + con + 3 * logn 
    } else  info <- rbind(info, c(0, 0, 0))  
    if ( !is.null(model) ) {
	options(warn = -1)
      xz <- as.vector( cor(z, x) )
      yx.z <- ( yx - xz * r ) / sqrt(1 - xz^2) / sqrt(1 - r^2)
      sel <- which.max( abs(yx.z) )
      r <- yx.z[sel]
      stat <- 0.5 * abs( log( (1 + r) / (1 - r) ) ) * sqrt(n - 4)    
      pv <- log(2) + pt(stat, n - 4, lower.tail = FALSE, log.p = TRUE)  ## logged p-values  
      if ( pv < threshold )  {
        model <- .lm.fit( cbind(z, x[, sel]), y )
        tool[2] <-   n * log( sum(model$residuals^2)/n ) + con + 4 * logn
        if ( tool[1] - tool[2] > tolb ) {
          info <- rbind(info, c(sel, pv, stat) )
	    z <- cbind(z, x[, info[1:2, 1] ])
          x[, info[1:2, 1] ] <- 0  
        } else  info <- rbind(info, c(0, 0, 0)) 
      } else  info <- rbind(info, c(0, 0, 0)) 

    }
    k <- 2
  
    if ( info[2, 1] > 0 ) { 
      while ( info[k, 2] < threshold  &  k < n - 20  &  tool[ k - 1 ] - tool[ k ] > tolb  &  k < p )  {
        sela <- info[, 1]
        m <- n - 3 - k 
        k <- k + 1	
        e1 <- model$residuals
        e2 <- .lm.fit(z, x)$residuals
        ## yx.z <- colsums(e1 * e2) / sqrt( Rfast::colsums(e2^2) * sum(e1^2) ) 
		options(warn = -1)
        yx.z <- cor(e2, e1) 
        sel <- which.max( abs(yx.z) )  
        r <- yx.z[sel]
        stat <- abs( 0.5 * log( (1 + r) / (1 - r) ) * sqrt(m) )  ## 
        pv <- log(2) + pt(stat, m, lower.tail = FALSE, log.p = TRUE)  ## logged p-values  
        if ( pv < threshold )  {
          z <- cbind(z, x[, sel])
          model <- .lm.fit(z, y )
          tool[k] <- n * log( sum(model$residuals^2)/n ) + con + (k + 2) * logn
          if ( tool[k - 1] - tool[k] > tolb ) {
            info <- rbind(info, c(sel, pv, stat) )
            x[, sel] <- 0  
          } else  info <- rbind(info, c(0, 0, 0)) 
        } else  info <- rbind(info, c(0, 0, 0)) 

      } 
    }
    
    info <- cbind(info, tool[1:k])
    colnames(info)[4] <- "bic"
    info <- info[1:c(k-1), , drop = FALSE]   

  #############################
  ###### Adjusted R-squared stopping criterion
  #############################
  } else if ( stopping == "ar2" ) {
    tool <- numeric( min(n, p) ) 
    info <- cbind(0, 0, 0)
    if ( pv < threshold ) {
      info <- cbind(sel, pv, stat)
      z <- x[, sel, drop = FALSE]
      model <- .lm.fit(z, y)
      r2 <- 1 - cor(y, model$residuals )^2
      tool[1] <- 1 - (1 - r2) * (n - 1 ) / ( n - 2)
    } else  info <- rbind(info, c(0, 0, 0))
    if ( !is.null(model) ) {
	  options(warn = -1)
      xz <- as.vector( cor(z, x) )
      yx.z <- ( yx - xz * r ) / sqrt(1 - xz^2) / sqrt(1 - r2)
      sel <- which.max( abs(yx.z) )
      r <- yx.z[sel]
      stat <- 0.5 * abs( log( (1 + r) / (1 - r) ) ) * sqrt(n - 4)    
      pv <- log(2) + pt(stat, n - 4, lower.tail = FALSE, log.p = TRUE)  ## logged p-values  
      if ( pv < threshold )  {
        z <- cbind(z, x[, sel ])
        model <- .lm.fit(z, y ) 
        r2 <- 1 - cor(y, model$residuals )^2
        tool[2] <- 1 - (1 - r2) * (n - 1) / ( n - 3)
          if ( tool[2] - tool[1] > tolr ) {
          info <- rbind(info, c(sel, pv, stat) )
	    z <- cbind(z, x[, info[1:2, 1] ])
          x[, info[1:2, 1] ] <- 0  
        } else  info <- rbind(info, c(0, 0, 0))  
       } else  info <- rbind(info, c(0, 0, 0)) 

    }
    k <- 2
  
    if ( info[2, 1] > 0 ) { 
      while ( info[k, 2] < threshold  &  k < n - 20  &  tool[ k ] - tool[ k - 1 ] > tolr  &  k < p )  {
        sela <- info[, 1]
        m <- n - 3 - k
        k <- k + 1
        e1 <- model$residuals
        e2 <- .lm.fit(z, x)$residuals
        ## yx.z <- colsums(e1 * e2) / sqrt( colsums(e2^2) * sum(e1^2) ) 
		options(warn = -1)
        yx.z <- cor(e2, e1) 
        sel <- which.max( abs(yx.z) )  
        r <- yx.z[sel]
        stat <- abs( 0.5 * log( (1 + r) / (1 - r) ) * sqrt(m) )  ## 
        pv <- log(2) + pt(stat, m, lower.tail = FALSE, log.p = TRUE)  ## logged p-values  
        if ( pv < threshold )  {
          z <- cbind(z, x[, sel])
          model <- .lm.fit(z, y ) 
          r2 <- 1 - cor(y, model$residuals )^2
          tool[k] <- 1 - (1 - r2) * (n - 1) / ( n - k - 1)
          if ( tool[k] - tool[k - 1] > tolr ) {
            info <- rbind(info, c(sel, pv, stat) )
            x[, sel] <- 0 
          } else  info <- rbind(info, c(0, 0, 0)) 
        } else  info <- rbind(info, c(0, 0, 0)) 

      } 
    }
    info <- cbind(info, tool[1:k])
    colnames(info)[4] <- "adjusted R2"
    info <- info[1:c(k-1), , drop = FALSE]   

  #############################
  ###### BIC and adjusted R-squared stopping criterion
  #############################
  } else if ( stopping == "BICR2" ) {
    info <- cbind(0, 0, 0, 0)
    toolb <- toolr <- numeric( min(n, p) )
    if ( pv < threshold ) {
      info <- cbind(sel, pv, stat)
      z <- x[, sel, drop = FALSE]
      model <- .lm.fit(z, y)   
      options(warn = -1)
      r2 <- 1 - cor(y, model$residuals )^2
      toolr[1] <- 1 - (1 - r2) * (n - 1 ) / ( n - 2)
      toolb[1] <- n * log( sum(model$residuals^2)/n ) + con + 3 * logn 
    } else  info <- rbind(info, c(0, 0, 0))
      if ( !is.null(model) ) {
	  options(warn = -1)
        xz <- as.vector( cor(z, x) )
        yx.z <- ( yx - xz * r ) / sqrt(1 - xz^2) / sqrt(1 - r2)
        sel <- which.max( abs(yx.z) )
        r <- yx.z[sel]
        stat <- 0.5 * abs( log( (1 + r) / (1 - r) ) ) * sqrt(n - 4)    
        pv <- log(2) + pt(stat, n - 4, lower.tail = FALSE, log.p = TRUE)  ## logged p-values  
        if ( pv < threshold )  {
          z <- cbind(z, x[, sel])
          model <- .lm.fit(z, y ) 
          r2 <- 1 - cor(y, model$residuals )^2
          toolr[2] <- 1 - (1 - r2) * (n - 1) / ( n - 3)
          toolb[2] <-  n * log( sum( model$residuals^2)/n ) + con + 4 * logn
          if ( toolb[1] - toolb[2] > tolb  &  toolr[2] - toolr[1] > tolr ) {
            info <- rbind(info, c(sel, pv, stat) )
	      z <- cbind(z, x[, info[1:2, 1] ])
            x[, info[1:2, 1] ] <- 0 
          } else  info <- rbind(info, c(0, 0, 0))  
        } else  info <- rbind(info, c(0, 0, 0)) 
    }
    k <- 2
  
    if ( info[2, 1] > 0 ) { 
      while ( info[k, 2] < threshold  &  k < n - 20 &  toolb[ k - 1 ] - toolb[ k ] > tolb  & toolr[ k ] - toolr[ k - 1 ] > tolr  &  k < p )  {
        sela <- info[, 1]
        m <- n - 3 - k
        k <- k + 1
        e1 <- model$residuals
        e2 <- .lm.fit(z, x)$residuals
        ## yx.z <- Rfast::colsums(e1 * e2) / sqrt( Rfast::colsums(e2^2) * sum(e1^2) ) 
		options(warn = -1)
        yx.z <- cor(e2, e1) 
        sel <- which.max( abs(yx.z) )  
        r <- yx.z[sel]
        stat <- abs( 0.5 * log( (1 + r) / (1 - r) ) * sqrt(m) )  ## 
        pv <- log(2) + pt(stat, m, lower.tail = FALSE, log.p = TRUE)  ## logged p-values  
        if ( pv < threshold )  {
          z <- cbind(z, x[, sel])
          model <- .lm.fit(z, y ) 
          r2 <- 1 - cor(y, model$residuals )^2
          toolr[k] <- 1 - (1 - r2) * (n - 1) / ( n - k - 1)
          toolb[k] <-  n * log( sum(model$residuals^2)/n ) + con + (k + 2) * logn
          if ( toolb[k - 1] - toolb[k] > tolb  &  toolr[k] - toolr[k - 1] > tolr ) {
            info <- rbind(info, c(sel, pv, stat) )
            x[, sel] <- 0 
          } else  info <- rbind(info, c(0, 0, 0)) 
        } else  info <- rbind(info, c(0, 0, 0)) 

      } 
    }

    info <- cbind(info, toolb[1:k], toolr[1:k])
    colnames(info)[4:5] <- c("bic", "adjusted R2")
    info <- info[1:c(k-1), , drop = FALSE]   

  }

  info
}