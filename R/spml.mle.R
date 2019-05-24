#[export]
spml.mle<-function(x, tol = 1e-09, maxiters = 100) {
  .Call(Rfast_spml_mle,as.matrix(x),tol,maxiters)
}
