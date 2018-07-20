
is.symmetric <- function(x) {
  .Call(Rfast_symmetric,x)
}