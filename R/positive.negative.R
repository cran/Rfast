

positive.negative <- function(x,method = "min"){
    .Call("Rfast_positive_negative",PACKAGE = "Rfast",x,method)
}