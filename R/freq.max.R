
freq.max <- function(x,na.rm = FALSE) {
	if(is.integer(x)){
  		.Call('Rfast_max_freq_i',PACKAGE = "Rfast",x,na.rm)
	}else{
		.Call('Rfast_max_freq_d',PACKAGE = "Rfast",x,na.rm)
	}
}