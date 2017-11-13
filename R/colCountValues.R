
colCountValues<-function(x,values,parallel = FALSE){
	if(parallel){
		.Call('Rfast_col_count_values_p', PACKAGE = 'Rfast',x,values)
	}else{
		.Call('Rfast_col_count_values', PACKAGE = 'Rfast',x,values)
	}
}