
Dist <- function(x,method = "euclidean", square = FALSE,p=0) {
  x <- t(x)
  if(method == "euclidean" || p==2){
  	return (.Call('Rfast_euclidean_dist',x,square))
  }else if(method == "manhattan" || p==1){
  	return (.Call('Rfast_manhattan_dist',x))
  }else if(method == "maximum"){
  	return (.Call('Rfast_max_dist',x))
  }else if(method == "minimum"){
  	return (.Call('Rfast_min_dist',x))
  }else if(method == "canberra1"){
  	return (.Call('Rfast_canberra1_dist',x))
  }else if(method == "canberra2"){
  	return (.Call('Rfast_canberra2_dist',x))
  }else if(method == "minkowski"){
  	return (.Call('Rfast_minkowski_dist',x,p))
  }else if(method == "bhattacharyya"){
    return (.Call('Rfast_bhattacharyya_dist',x))
  }else if(method == "hellinger"){
    return (.Call('Rfast_hellinger_dist',sqrt(x),square))
  }else if(method == "total_variation"){
    return (.Call('Rfast_total_variation_dist',x))
  }else if(method == "kullback_leibler" || method == "jensen_shannon"){
    return (.Call('Rfast_kullback_leibler_dist',x))
  }else {
  	stop(paste("Unsupported Method: ",method))
  }
}