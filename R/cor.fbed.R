cor.fbed <- function(y, x, alpha = 0.05, K = 0) {
  quan <- 1 - alpha/2
  sig <- log(alpha)
  dm <- dim(x)
  n <- dm[1]
  p <- dm[2]
  ind <- 1:p
  sela <- NULL
  card <- 0
  sa <- NULL
  pva <- NULL

  runtime <- proc.time()
  x <- Rfast::standardise(x, center = TRUE, scale = FALSE)
  y <- y - mean(y)
  options(warn = -1)
  yx <- as.vector( cor(y, x) )
  n.tests <- p
  stat <- abs( 0.5 * log( (1 + yx) / (1 - yx) ) * sqrt(n - 3) ) 
  critvalue <- qt(quan, n - 3)
  s <- which(stat > critvalue)
  
  if ( length(s) > 0 ) {
    sel <- which.max(stat) 
    sela <- sel
    s <- s[ - which(s == sel) ]
    r <- yx[sel]
    pv <- log(2) + pt(stat[sel], n - 2, lower.tail = FALSE, log.p = TRUE)   
    pva <- pv 
    sa <- stat[sel] 
    stat <- numeric(p)
    z <- x[, sel]
    
    if ( length(s) > 0 ) {
      options(warn = -1)
      xz <- as.vector( cor(z, x[, ind[s] ]) )
      n.tests <- n.tests + length( ind[s] )
      yx.z <- abs( ( yx[ ind[s] ] - xz * r ) / sqrt(1 - xz^2) / sqrt(1 - r^2) ) 
      stat[ ind[s] ] <- abs( 0.5 * log( (1 + yx.z) / (1 - yx.z) ) * sqrt(n - 4) )
      critvalue <- qt(quan, n - 4)
      s <- which( stat > critvalue )
      if ( length(s) > 0 ) {
        sel <- which.max(stat)
        pv <- log(2) + pt(stat[sel], n - 4, lower.tail = FALSE, log.p = TRUE) 
        sa <- c(sa, stat[sel]) 
        pva <- c(pva, pv)
        sela <- c(sela, sel[sel > 0] )
        s <- s[ - which(s == sel) ]
        z <- cbind(z, x[, sel])
      }  ## end if ( length(s) > 0 )
      ######################
      while ( sum(s > 0) > 0 )  {
        stat <- numeric(p)
        m <- n - 3 - length(sela)
        #e1 <- .lm.fit(z, y)$residuals
        #e2 <- .lm.fit(z, x[, ind[s] ])$residuals
        er <- .lm.fit(z, cbind(y, x[, ind[s] ]) )$residuals
        options(warn = -1)
        #r <- cor(e2, e1) 
        r <- cor(er[, 1], er[, -1])
        n.tests <- n.tests + length( ind[s] )
        stat[ ind[s] ] <- abs( 0.5 * log( (1 + r) / (1 - r) ) * sqrt(m) )    
        critvalue <- qt(quan, m)
        s <- which( stat > critvalue )
        sel <- which.max(stat) * ( length(s) > 0 )
        pv <- log(2) + pt(stat[sel], m, lower.tail = FALSE, log.p = TRUE) 
        sa <- c(sa, stat[sel]) 
        pva <- c(pva, pv)
        sela <- c(sela, sel[sel > 0] )
        s <- s[ - which(s == sel) ]
        z <- cbind(z, x[, sel])
      }  ## end while( sum(s > 0) > 0 )
    }  ##  end if ( length(s) > 0 )
  card <- sum(sela > 0)

  if ( K == 1) {
    stat <- numeric(p)
    m <- n - 3 - length(sela)
    e1 <- .lm.fit(z, y)$residuals
    e2 <- .lm.fit(z, x[, ind[-sela] ])$residuals
    options(warn = -1)
    r <- cor(e2, e1) 
    n.tests[2] <- length( ind[-sela] )
    stat[ ind[-sela] ] <- abs( 0.5 * log( (1 + r) / (1 - r) ) * sqrt(m) )    
    critvalue <- qt(quan, m)
    s <- which( stat > critvalue )
    sel <- which.max(stat) * ( length(s) > 0 )
    pv <- log(2) + pt(stat[sel], m, lower.tail = FALSE, log.p = TRUE) 
    sa <- c(sa, stat[sel]) 
    pva <- c(pva, pv)
    sela <- c(sela, sel[sel > 0] )
    s <- s[ - which(s == sel) ]
    z <- cbind(z, x[, sel])     
    while ( sum(s > 0) > 0 ) {
      stat <- numeric(p)
      m <- n - 3 - length(sela)
      e1 <- .lm.fit(z, y)$residuals
      e2 <- .lm.fit(z, x[, ind[s] ])$residuals
      options(warn = -1)
      r <- cor(e2, e1) 
      n.tests[2] <- n.tests[2] + length( ind[s] )
      stat[ ind[ s ] ] <- abs( 0.5 * log( (1 + r) / (1 - r) ) * sqrt(m) )    
      critvalue <- qt(quan, m)
      s <- which( stat > critvalue )
      sel <- which.max(stat) * ( length(s) > 0 )
      pv <- log(2) + pt(stat[sel], m, lower.tail = FALSE, log.p = TRUE) 
      sa <- c(sa, stat[sel]) 
      pva <- c(pva, pv)
      sela <- c(sela, sel[sel > 0] )
      s <- s[ - which(s == sel) ]
      z <- cbind(z, x[, sel])     
    } ## end while ( sum(s > 0) > 0 ) 
    card <- c(card, sum(sela > 0) )  
  } ## end  if (K == 1)
 
  if ( K > 1  )  {
    stat <- numeric(p)
    m <- n - 3 - length(sela)
    e1 <- .lm.fit(z, y)$residuals
    e2 <- .lm.fit(z, x[, ind[-sela] ])$residuals
    options(warn = -1)
    r <- cor(e2, e1) 
    n.tests[2] <- length( ind[-sela] )
    stat[ ind[-sela] ] <- abs( 0.5 * log( (1 + r) / (1 - r) ) * sqrt(m) )    
    critvalue <- qt(quan, m)
    s <- which( stat > critvalue )
    sel <- which.max(stat) * ( length(s) > 0 )
    pv <- log(2) + pt(stat[sel], m, lower.tail = FALSE, log.p = TRUE) 
    sa <- c(sa, stat[sel]) 
    pva <- c(pva, pv)
    sela <- c(sela, sel[sel > 0] )
    s <- s[ - which(s == sel) ]
    z <- cbind(z, x[, sel])     
    while ( sum(s > 0) > 0 ) {
      stat <- numeric(p)
      m <- n - 3 - length(sela)
      e1 <- .lm.fit(z, y)$residuals
      e2 <- .lm.fit(z, x[, ind[s] ])$residuals
      options(warn = -1)
      r <- cor(e2, e1) 
      n.tests[2] <- n.tests[2] + length( ind[s] )
      stat[ ind[s] ] <- abs( 0.5 * log( (1 + r) / (1 - r) ) * sqrt(m) )    
      critvalue <- qt(quan, m)
      s <- which( stat > critvalue )
      sel <- which.max(stat) * ( length(s) > 0 )
      pv <- log(2) + pt(stat[sel], m, lower.tail = FALSE, log.p = TRUE) 
      sa <- c(sa, stat[sel]) 
      pva <- c(pva, pv)
      sela <- c(sela, sel[sel > 0] )
      s <- s[ - which(s == sel) ]
      z <- cbind(z, x[, sel])     
    } ## end while ( sum(s > 0) > 0 ) 
    card <- c(card, sum(sela > 0) )  

    vim <- 1
    while ( vim < K  & card[vim + 1] - card[vim] > 0 ) {
      stat <- numeric(p)
      vim <- vim + 1
      m <- n - 3 - length(sela)
      e1 <- .lm.fit(z, y)$residuals
      e2 <- .lm.fit(z, x[, ind[-sela] ])$residuals
      options(warn = -1)
      r <- cor(e2, e1) 
      n.tests[vim + 1] <- length( ind[-sela] )
      stat[ ind[-sela] ] <- abs( 0.5 * log( (1 + r) / (1 - r) ) * sqrt(m) )    
      critvalue <- qt(quan, m)
      s <- which( stat > critvalue )
      sel <- which.max(stat) * ( length(s) > 0 )
      pv <- log(2) + pt(stat[sel], m, lower.tail = FALSE, log.p = TRUE) 
      sa <- c(sa, stat[sel]) 
      s <- s[ - which(s == sel) ]
      pva <- c(pva, pv)
      sela <- c(sela, sel[sel > 0] )
      z <- cbind(z, x[, sel])     
      while ( sum(s > 0) > 0 ) {
        stat <- numeric(p)
        m <- n - 3 - sum(sela)
        e1 <- .lm.fit(z, y)$residuals
        e2 <- .lm.fit( z, x[, ind[s] ] )$residuals
        options(warn = -1)
        r <- cor(e2, e1) 
        n.tests[vim + 1] <- n.tests[vim + 1] + length( ind[s] )
        stat[ ind[s] ] <- abs( 0.5 * log( (1 + r) / (1 - r) ) * sqrt(m) )    
        critvalue <- qt(quan, m)
        s <- which( stat > critvalue )
        sel <- which.max(stat) * ( length(s) > 0 )
        pv <- log(2) + pt(stat[sel], m, lower.tail = FALSE, log.p = TRUE) 
        sa <- c(sa, stat[sel]) 
        pva <- c(pva, pv)
        sela <- c(sela, sel[sel > 0] )
        s <- s[ - which(s == sel) ]
        z <- cbind(z, x[, sel])     
      } ## end while ( length(s) > 0 )
      card <- c(card, sum(sela > 0) )  
    }  ## end while ( vim < K )
  }  ## end if ( K > 1)
  } ## end if ( length(s) > 0 )
     
  runtime <- proc.time() - runtime
  len <- sum( sela > 0 )
  if (len > 0) {
    res <- cbind(sela[1:len], sa[1:len], pva[1:len] )
    info <- matrix(nrow = length(card), ncol = 2)
    info[, 1] <- card
    info[, 2] <- n.tests
  } else {
    res <- matrix(c(0, 0, 0), ncol = 3)
    info <- matrix(c(0, p), ncol = 2)
  }  
  colnames(res) <- c("Vars", "stat", "log p-value")
  rownames(info) <- paste("K=", 1:length(card)- 1, sep = "")
  colnames(info) <- c("Number of vars", "Number of tests")
  list(runtime = runtime, res = res, info = info)
}






