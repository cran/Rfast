
Norm<- function(x, type = "F") {
  .Call("Rfast_Norm",PACKAGE = "Rfast",x,type)
}