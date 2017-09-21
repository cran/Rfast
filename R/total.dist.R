


total.dist <- function(x,method = "euclidean", square = FALSE,p=0) {
  x <- t(x)
  if(method == "hellinger"){
    x <- sqrt(x)
  }
  .Call("Rfast_total_dists",PACKAGE = "Rfast",x,method,square,p)
}