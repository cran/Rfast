kruskaltests <- function(x, ina, logged = FALSE) { 
  n <- length(ina)
  R <- colRanks(x)
  ni <- tabulate(ina)
  ni <- ni[ni > 0]
  up <- colsums( rowsum(R, ina)^2 / ni ) - n * (n + 1)^2 / 4
  down <- colsums(R^2) - n * (n + 1)^2 / 4
  stat <- (n - 1) * up / down
  pvalue <- pchisq(stat, length(ni) - 1, lower.tail = FALSE, log.p = logged) 
  cbind(stat, pvalue)
}
