rint.regbx <- function(y, x, id) {
  mod <- Rfast::lmfit(cbind(1, x), y) 
  be <- mod$be
  e <- mod$residuals
  N <- length(y)
  n <- Rfast::sort_unique.length(id)
  d <- N / n
  f <- 1 - 1/d
  seid <- rowsum(e^2, id)
  myid <- rowsum(y, id)/ d
  my <- sum(myid) / n
  com <- (myid - my)^2
  sml <- sum(seid - d * com )/N/f
  dml <- sum(com) / n /sml  - 1/ d
  tauml <- dml * sml
  com <- rowsum(e, id)
  ranef <- tauml /( tauml + sml/d ) * com / d
  sz <- sum(seid)
  sz2 <- sum(com^2) 
  loglik <- n * d * log(sml) + n * log1p(d * tauml / sml) + sz/sml - tauml / (sml^2 + d * tauml * sml) * sz2
  loglik <-  -0.5 * loglik - n * d/2 * log(2 * pi) 
  dev <-  -2 * loglik
  info <- c(tauml, sml, loglik, dev, dev + 4 * log(N) )
  names(info) <- c("sigma_tau", "sigma_errors", "log-likelihood", "deviance", "BIC")
  list(info = info, be = be, ranef = ranef)
}
