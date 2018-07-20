regression <- function(x, y, logged = FALSE) {
  if ( is.matrix(x) ) {
    n <- length(y)
    rho <- as.vector( cor(y, x) )
    sqdof <- sqrt(n - 2)
    stat <- rho * sqdof/sqrt(1 - rho^2)
    if (logged) {
      pvalue <- log(2) + pt(abs(stat), n - 2, lower.tail = FALSE, log.p = TRUE)
    } else  pvalue <- 2 * pt(abs(stat), n - 2, lower.tail = FALSE)
  } else {  ## not matrix now
    poia <- Rfast::which.is(x)
    if ( length(poia) == 0 ) {
      n <- length(y)
      rho <- as.vector( cor(y, x) )
      sqdof <- sqrt(n - 2)
      stat <- rho * sqdof/sqrt(1 - rho^2)
      if (logged) {
        pvalue <- log(2) + pt(abs(stat), n - 2, lower.tail = FALSE, log.p = TRUE)
      } else  pvalue <- 2 * pt(abs(stat), n - 2, lower.tail = FALSE)
    } else {  ## some are factors and some are numericals
      dm <- dim(x)
      n <- dm[1]
      D <- dm[2]
      stat <- numeric(D)
      pvalue <- numeric(D)
      n <- length(y)
      rho <- as.vector( cor(y, x[, -poia]) )
      sqdof <- sqrt(n - 2)
      stat[-poia] <- rho * sqdof/sqrt(1 - rho^2)
      if (logged) {
        pvalue[-poia] <- log(2) + pt(abs(stat[-poia]), n - 2, lower.tail = FALSE, log.p = TRUE)
      } else  pvalue[-poia] <- 2 * pt(abs(stat[-poia]), n - 2, lower.tail = FALSE)
	   mod <- Rfast::colanovas(y, x[, poia, drop = FALSE])
	   stat[poia] <- mod[, 1]
       pvalue[poia] <- mod[, 2]  	   
    }  
  }  ## end if is.matrix(x)
  cbind(stat, pvalue)
}