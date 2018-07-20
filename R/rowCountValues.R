
rowCountValues<-function(x,values,parallel = FALSE){
	if(parallel){
		.Call(Rfast_row_count_values_p,x,values)
	}else{
		.Call(Rfast_row_count_values,x,values)
	}
}