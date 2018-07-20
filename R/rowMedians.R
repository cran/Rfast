
rowMedians <- function(x,parallel = FALSE) {
  	if(parallel){
		.Call(Rfast_row_meds_p,x)
	}else{
		.Call(Rfast_row_meds,x)
	}
}