rm.anovas <- function(y, x, logged = FALSE) {
  d <- length(x)
  n <- dim(y)[1]/d
  ina <- rep(1:n, each = d)
  xi <- rep(x, n)
  yi <- rowsum(y, ina)/d
  yj <- rowsum(y, xi)/n
  yt <- Rfast::colmeans(y)
  sst <- n * Rfast::colsums( (yj - yt)^2 )
  #sss <- d * colsums( (yi - yt)^2 )
  yi <- rep(yi, each = d)
  yj <- rep(yj, n)
  ssr <- Rfast::colsums( (y - yi - yj + yt)^2 )
  dft <- d - 1
  dfs <- n - 1
  dfr <- dft * dfs
  mst <- sst / dft
  #mss <- ssr / dfs
  msr <- ssr / dfr
  stat <- mst/msr
  pvalue <- pf(stat, dft, dfr, lower.tail = FALSE, log.p = logged) 
  cbind(stat, pvalue)
}