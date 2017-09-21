

dvar <- function(x) {
  .Call("Rfast_dvar",PACKAGE = "Rfast",t(x))
}