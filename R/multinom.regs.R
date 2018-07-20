multinom.regs <- function(y, x, tol = 1e-08, logged = FALSE, maxiters = 100) {
  if ( !is.numeric(y) )  y <- as.numeric(y)
  mod <- .Call(Rfast_multinom_regs,y, x, tol,logged, maxiters )
  colnames(mod) <- c("stat", "pval")
  mod
}
