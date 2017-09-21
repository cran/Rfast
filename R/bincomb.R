

bincomb <- function(n) {
  .Call("Rfast_bincomb",PACKAGE = "Rfast",n)
}