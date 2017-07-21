
count_value <- function(x,value) {
	if(is.character(x)){
  		.Call('Rfast_count_value_string', PACKAGE = 'Rfast',x,value)
  	}else{
  		.Call('Rfast_count_value', PACKAGE = 'Rfast',x,value)
  	}
}