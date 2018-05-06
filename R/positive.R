

positive <- function(x,method = "min"){
    .Call("Rfast_positive",PACKAGE = "Rfast",x,method)
}