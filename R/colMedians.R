
colMedians <- function(x,na.rm=FALSE,parallel = FALSE) {
	if(parallel){
		.Call(Rfast_col_meds_p,x,na.rm)
	}else{
		.Call(Rfast_col_meds,x,na.rm)
	}
}