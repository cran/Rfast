spml.mle<-function(x, tol = 1e-09, maxiters = 100) {
  .Call("Rfast_spml_mle",PACKAGE = "Rfast",x,tol,maxiters)
}
