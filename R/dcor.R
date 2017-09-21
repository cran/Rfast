

dcor <- function(x,y) {
  .Call("Rfast_dcor",PACKAGE = "Rfast",t(x),t(y))
}