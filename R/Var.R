
Var <- function(x,std = FALSE) {
  f <- .Call('Rfast_var_c', PACKAGE = 'Rfast',x)
  if(std) f <- sqrt(f)
  f
}