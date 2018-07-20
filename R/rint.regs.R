rint.regs <- function(y, x, id, tol = 1e-08, logged = FALSE, parallel = FALSE, maxiters = 100) {
  mod <- .Call(Rfast_rint_regs, x, y, id, tol, logged, parallel, maxiters)
  colnames(mod) < c("stat", "pval")
  mod
}
