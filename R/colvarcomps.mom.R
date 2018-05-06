colvarcomps.mom <- function(x, id, parallel = FALSE) {
  k <- Rfast::sort_unique.length(id) 
  ni <- tabulate(id)
  ni <- ni[ni > 0]
  sam <- length(ni)
  n <- dim(x)[1]
  sx2 <- Rfast::colsums(x^2, parallel = parallel)
  m <- rowsum(x, id)
  a <- Rfast::colsums(m^2/ni)
  b <- Rfast::colsums(m)^2/n
  df1 <- k - 1
  df2 <- n - k   
  mst <- (a - b)/df1       ### /(k - 1)
  mse <- (sx2 - a)/df2   ##  /(n - k)
  fa <- mst / mse
  ranvar <- (mst - mse)/sam  
  ranvar[ranvar <= 0 ] <- 0
  rat <- ranvar / (ranvar + mse)
  L <- (fa *  qf(0.025, df2, df1) - 1) / sam
  U <- (fa * qf(0.975, df2, df1) - 1 ) / sam
  l <- L / (1 + L)
  u <- U / (1 + U)
  res <- cbind(ranvar, mse, rat, l, u)
  colnames(res) <- c("ranvar", "MSE", "ratio", "2.5% lower", "97.5% upper")
  res
}
