kruskaltests <- function(x, ina, logged = FALSE) { 
  n <- dim(x)[1]
  R <- apply(x, 2, rank)
  ni <- tabulate(ina)
  up <- colsums( rowsum(R, ina)^2 / ni ) - n * (n + 1)^2 / 4
  down <- colsums(R^2) - n * (n + 1)^2 / 4
  stat <- (n - 1) * up / down
  pvalue <- pchisq(stat, length(ni) - 1, lower.tail = FALSE, log.p = logged) 
  cbind(stat, pvalue)
}
