
freq.max <- function(x,na.rm = FALSE) {
	if(is.integer(x)){
  		.Call(Rfast_max_freq_i,x,na.rm)
	}else{
		.Call(Rfast_max_freq_d,x,na.rm)
	}
}