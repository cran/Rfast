spml.regs <- function(y, x, tol = 1e-07, logged = FALSE, maxiters = 100, parallel = FALSE) {
  res <- .Call(Rfast_spml_regs, y,x,tol,logged,maxiters,parallel)
  colnames(res) <- c("statistic", "p-value")
  res
}
