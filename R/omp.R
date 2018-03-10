omp <- function (y, x, tol = qchisq(0.95, 1) + log( length(y) ), type = "logistic" ) {
    tic <- proc.time()
    dm <- dim(x)
    d <- dm[2]
    n <- dm[1]
    ind <- 1:d
    x <- standardise(x)

    if ( type == "logistic" ) {
      p <- sum(y)/n
      rho <-  -2 * (n * p * log(p) + (n - n * p) * log(1 - p))
      ela <- as.vector(cor(y - p, x) )
      sel <- which.max( abs(ela) )
      sela <- sel
      names(sela) <- NULL
      options(warn = -1)
      mod <- glm_logistic(x[, sel], y)
      est <- exp( - mod$be[1] - x[, sel] * mod$be[2] )
      res <-  y - 1/ (1 + est)
      rho[2] <- mod$devi
      ind[sel] <- 0
      i <- 2
      r <- numeric(d)
      while ( (rho[i - 1] - rho[i]) > tol ) {
        i <- i + 1
        r[ind] <- colsums(res * x[, ind] )
        sel <- which.max( abs(r) )
        sela <- c(sela, sel)
        options(warn = -1)
        mod <- glm_logistic(x[, sela], y)        
        est <- as.vector( exp( -mod$be[1] - x[, sela] %*% mod$be[-1] ) )
        res <-  y - 1/ (1 + est)
        rho[i] <- mod$devi
        ind[sela] <- 0
        r[sela] <- 0
      }

    } else if ( type == "poisson" ) {
      m <- sum(y)/n
      rho <- 2 * sum(y * log(y), na.rm = TRUE) - 2 * n * m * log(m)
      ela <- as.vector( cor(y - m, x) )
      sel <- which.max( abs(ela) )
      sela <- sel
      names(sela) <- NULL
      options(warn = -1)
      mod <- glm_poisson(x[, sel], y)
      res <- y - exp( mod$be[1] + x[, sel] * mod$be[2] )
      rho[2] <- mod$devi
      ind[sel] <- 0
      i <- 2
      r <- numeric(d)
      while ( (rho[i - 1] - rho[i]) > tol ) {
        i <- i + 1
        r[ind] <- colsums(res * x[, ind] )
        sel <- which.max( abs(r) )
        sela <- c(sela, sel)
        options(warn = -1)
        mod <- glm_poisson(x[, sela], y)        
        res <- y - as.vector( exp( mod$be[1] + x[, sela] %*% mod$be[-1] ) ) 
        rho[i] <- mod$devi
        ind[sela] <- 0
        r[sela] <- 0
      }

    } else if ( type == "quasipoisson" ) {
      m <- sum(y)/n
      rho <- 2 * sum(y * log(y), na.rm = TRUE) - 2 * n * m * log(m)
      ela <- as.vector( cor(y - m, x) )
      sel <- which.max( abs(ela) )
      sela <- sel
      names(sela) <- NULL
      options(warn = -1)
      mod <- qpois.reg(x[, sel], y)
	phi <- mod$phi
      res <- y - exp( mod$be[1] + x[, sel] * mod$be[2] )
      rho[2] <- mod$devi
      ind[sel] <- 0
      i <- 2
      r <- numeric(d)
      while ( (rho[i - 1] - rho[i]) / phi > tol ) {
        i <- i + 1
        r[ind] <- colsums(res * x[, ind] )
        sel <- which.max( abs(r) )
        sela <- c(sela, sel)
        options(warn = -1)
        mod <- prop.reg(x[, sela], y, varb = "glm")        
        res <- y - as.vector( exp( mod$be[1] + x[, sela] %*% mod$be[-1] ) ) 
        rho[i] <- mod$devi
	  phi <- mod$phi 
        ind[sela] <- 0
        r[sela] <- 0
      }
	  
    } else if ( type == "quasibinomial" ) {
      m <- sum(y)/n
      rho <- 2 * sum(y * log(y), na.rm = TRUE) - 2 * n * m * log(m)
      ela <- as.vector( cor(y - m, x) )
      sel <- which.max( abs(ela) )
      sela <- sel
      names(sela) <- NULL
      options(warn = -1)
      mod <- prop.reg(x[, sel], y, varb = "glm")
	phi <- mod$phi
      est <- exp( - mod$be[1] - x[, sel] * mod$be[2] )
      res <-  y - 1/ (1 + est)
      rho[2] <- mod$devi
      ind[sel] <- 0
      i <- 2
      r <- numeric(d)
      while ( (rho[i - 1] - rho[i]) / phi > tol ) {
        i <- i + 1
        r[ind] <- colsums(res * x[, ind] )
        sel <- which.max( abs(r) )
        sela <- c(sela, sel)
        options(warn = -1)
        mod <- prop.reg(x[, sela], y, varb = "glm")        
        est <- as.vector( exp( - mod$be[1] - x[, sela] %*% mod$be[-1] ) ) 
        res <- y - 1 / (1 + est) 
        rho[i] <- mod$devi
	  phi <- mod$phi 
        ind[sela] <- 0
        r[sela] <- 0
      }

    } else if ( type == "normlog" ) {
      ini <- normlog.mle(y)
      m <- ini$param[1]
      rho <- sum( (y - ini$param[1])^2 )
      ela <- cor(y - m, x)
      sel <- which.max( abs(ela) )
      sela <- sel
      names(sela) <- NULL
      options(warn = -1)
      mod <- normlog.reg(y, x[, sel])
      res <- y - exp( mod$be[1] + x[, sel] * mod$be[2] )
      rho[2] <- mod$deviance
	phi <- mod$devi/(n - 2)
      ind[sel] <- 0
      i <- 2
      r <- numeric(d)
      while ( (rho[i - 1] - rho[i]) / phi > tol ) {
        i <- i + 1
        r[ind] <- colsums(res * x[, ind] )
        sel <- which.max( abs(r) )
        sela <- c(sela, sel)
        options(warn = -1)
        mod <- normlog.reg(y, x[, sela])        
        res <- y - as.vector( exp( mod$be[1] + x[, sela] %*% mod$be[-1] ) ) 
        rho[i] <- mod$deviance
	  phi <- mod$deviance/(n - length(mod$be) )
        ind[sela] <- 0
        r[sela] <- 0
      }
	  
    } else if ( type == "weibull" ) {
      ini <- weibull.mle(y)
      m <- ini$param[2]
      rho <- 2 * ini$loglik
      ela <- as.vector( cor(y - m, x) )
      sel <- which.max( abs(ela) )
      sela <- sel
      names(sela) <- NULL
      options(warn = -1)
      mod <- weib.reg(y, x[, sel])
      res <- y - exp( mod$be[1] + x[, sel] * mod$be[2] )
      rho[2] <- 2 * mod$loglik
      ind[sel] <- 0
      i <- 2
      r <- numeric(d)
      while ( (rho[i] - rho[i - 1]) > tol ) {
        i <- i + 1
        r[ind] <- colsums(res * x[, ind] )
        sel <- which.max( abs(r) )
        sela <- c(sela, sel)
        options(warn = -1)
        mod <- weib.reg(y, x[, sela])        
        res <- y - as.vector( exp( mod$be[1] + x[, sela] %*% mod$be[-1] ) ) 
        rho[i] <- 2 * mod$loglik
        ind[sela] <- 0
        r[sela] <- 0
      }
	  
    } else if ( type == "mv" ) {
      con <- n * d * log(2 * pi / n) + n * d
      mod <- mvnorm.mle(y)
      rho <-  - 2 * mod$loglik
      res <- t(y) - mod$mu 
      ela <- numeric(d)
      for (i in 1:d)  ela[i] <- sum( (res %*% x[, i])^2 )
      sel <- which.max( ela )
      sela <- sel
      names(sela) <- NULL
      res <- .lm.fit( cbind(1, x[, sela]), y )$residuals
      rho[2] <- con + n * log( det( crossprod(res) ) )
      ind[sel] <- 0
      i <- 2
      r <- numeric(d)
      while ( (rho[i - 1] - rho[i]) > tol ) {
        i <- i + 1
        for (j in ind)   r[j] <- sum( crossprod(res, x[, j])^2 )
        sel <- which.max( r )
        sela <- c(sela, sel)
        res <- .lm.fit( cbind(1, x[, sela]), y )$residuals
        rho[i] <- con + n * log( det( crossprod(res) ) )
        ind[sela] <- 0
        r[sela] <- 0
      } 
	
	} 
	
    runtime <- proc.time() - tic
    len <- length(sela)
    info <- cbind(c(0, sela[-len]), rho[1:len])
    colnames(info) <- c("Selected Vars", "Deviance") 
    list(runtime = runtime, info = info)
}
