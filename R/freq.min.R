
freq.min <- function(x,na.rm = FALSE) {
	if(is.integer(x)){
  		.Call(Rfast_min_freq_i,x,na.rm)
	}else{
		.Call(Rfast_min_freq_d,x,na.rm)
	}
}