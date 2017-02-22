
Dist <- function(x,method = "euclidean", square = FALSE) {
  x <- t(x)
  if(method == "euclidean"){
  	return (.Call('Rfast_euclidean_dist', PACKAGE = 'Rfast',x,square))
  }else if(method == "manhattan"){
  	return (.Call('Rfast_manhattan_dist', PACKAGE = 'Rfast',x))
  }else if(method == "maximum"){
  	return (.Call('Rfast_max_dist', PACKAGE = 'Rfast',x))
  }
}