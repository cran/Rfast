#[export]
multinom.regs <- function(y, x, tol = 1e-08, logged = FALSE, parallel = FALSE, maxiters = 100) {
  if ( !is.numeric(y) )  y <- as.numeric(y)
  mod <- .Call("Rfast_multinom_regs",PACKAGE = "Rfast",y, x, tol,logged, parallel, maxiters )
  colnames(mod) <- c("stat", "pval")
  mod
}
