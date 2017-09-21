


Dist <- function(x,method = "euclidean", square = FALSE,p=0,vector = FALSE) {
  x <- t(x)
  if(method == "hellinger"){
    x <- sqrt(x)
  }
  if(vector){
  	.Call("Rfast_dist_vec",PACKAGE = "Rfast",x,method,square,p)
  }else{
  	.Call("Rfast_dist",PACKAGE = "Rfast",x,method,square,p)
  }
}