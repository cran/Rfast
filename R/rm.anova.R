rm.anova <- function(y, logged = FALSE) {
  dm <- dim(y)  
  d <- dim(y)[2]
  n <- dim(y)[1]
  ina <- rep(1:n, each = d)
  xi <- rep(1:d, n)
  yi <- Rfast::rowmeans(y)
  yj <- Rfast::colmeans(y)
  yt <- mean(yi)
  sst <- n * sum( (yj - yt)^2 )
  yi <- rep(yi, each = d)
  yj <- rep(yj, n)
  ssr <- sum( (as.vector( t(y) ) - yi - yj + yt)^2)
  dft <- d - 1
  dfs <- n - 1
  dfr <- dft * dfs
  mst <- sst/dft
  msr <- ssr/dfr
  stat <- mst/msr
  pvalue <- pf(stat, dft, dfr, lower.tail = FALSE, log.p = logged)
  c(stat, pvalue)
}

