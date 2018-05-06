
negative <- function(x,method = "min"){
    .Call("Rfast_negative",PACKAGE = "Rfast",x,method)
}