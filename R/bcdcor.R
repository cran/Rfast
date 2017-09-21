

bcdcor <- function(x,y) {
  .Call("Rfast_bcdcor",PACKAGE = "Rfast",t(x),t(y))
}