colrint.regbx <- function(y, x, id) {
  mod <- Rfast::lmfit(cbind(1, x), y) 
  be <- mod$be
  e <- mod$residuals
  N <- dim(y)[1]
  n <- Rfast::sort_unique.length(id)
  d <- N / n
  f <- 1 - 1/d
  seid <- rowsum(e^2, id)
  myid <- rowsum(y, id)/ d
  my <- Rfast::colsums(myid) / n
  com <- ( t(myid) - my )^2
  sml <- Rfast::rowsums( t(seid) - d * com )/N/f
  dml <- Rfast::rowsums(com) / n /sml  - 1/ d
  tauml <- dml * sml
  neg <- which( tauml < 0)
  if ( length(neg) >0 ) {
    sml[neg] <- sml[neg] + tauml[neg]
    tauml[neg] <- 0
  } 
  com <- rowsum(e, id)
  ranef <- tauml /( tauml + sml/d ) * t(com) / d
  sz <- Rfast::colsums(seid)
  sz2 <- Rfast::colsums(com^2) 
  loglik <- n * d * log(sml) + n * log1p(d * tauml / sml) + sz/sml - tauml / (sml^2 + d * tauml * sml) * sz2
  loglik <-  -0.5 * loglik - n * d/2 * log(2 * pi) 
  dev <-  -2 * loglik
  info <- cbind(tauml, sml, loglik, dev, dev + 4 * log(N) )
  colnames(info) <- c("sigma_tau", "sigma_errors", "log-likelihood", "deviance", "BIC")
  list(info = info, be = be, ranef = ranef)
}
