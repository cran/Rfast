

dcov <- function(x,y) {
  .Call("Rfast_dcov",PACKAGE = "Rfast",t(x),t(y))
}