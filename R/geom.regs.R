geom.regs <- function(y, x, tol = 1e-07, type = 1, logged = FALSE, parallel = FALSE, maxiters = 100) {
  mod <- .Call(Rfast_geom_regs,y, x, tol,logged, type,parallel,maxiters)
  colnames(mod) <- c("stat", "pval")
  mod
}
