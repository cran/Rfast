#[export]
med <- function(x,na.rm=FALSE) {
  .Call(Rfast_med,x,na.rm)
}

#[export]
colMedians <- function(x,na.rm=FALSE,parallel = FALSE) {
	if(parallel){
		.Call(Rfast_col_meds_p,x,na.rm)
	}else{
		.Call(Rfast_col_meds,x,na.rm)
	}
}

#[export]
rowMedians <- function(x,parallel = FALSE) {
  	if(parallel){
		.Call(Rfast_row_meds_p,x)
	}else{
		.Call(Rfast_row_meds,x)
	}
}