
colCountValues<-function(x,values,parallel = FALSE){
	if(parallel){
		.Call(Rfast_col_count_values_p,x,values)
	}else{
		.Call(Rfast_col_count_values,x,values)
	}
}