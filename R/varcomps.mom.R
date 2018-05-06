varcomps.mom <- function(x, ina) {
  ni <- tabulate(ina)
  k <- length(ni)
  n <- length(x)
  sx2 <- sum(x^2)
  m <- Rfast::group.sum(x, ina)
  a <- sum(m^2/ni)
  b <- sum(m)^2/n
  df1 <- k - 1
  df2 <- n - k   
  mst <- (a - b)/df1       ### /(k - 1)
  mse <- (sx2 - a)/df2   ##  /(n - k)
  fa <- mst / mse
  ranvar <- (mst - mse)/k  
  ranvar[ranvar <= 0 ] <- 0
  rat <- ranvar / (ranvar + mse)
  L <- (fa *  qf(0.025, df2, df1) - 1) / k
  U <- (fa * qf(0.975, df2, df1) - 1 ) / k
  l <- L / (1 + L)
  u <- U / (1 + U)
  res <- c(ranvar, mse, rat, l, u)
  names(res) <- c("ranvar", "MSE", "ratio", "2.5% lower", "97.5% upper")
  res
}
