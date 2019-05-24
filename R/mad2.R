#[export]
rowMads <- function(x,parallel = FALSE) {
	if(parallel){
		.Call(Rfast_row_mads_p,x)
	}else{
		.Call(Rfast_row_mads,x)
	}
}

#[export]
colMads <- function(x,parallel = FALSE) {
	if(parallel){
		.Call(Rfast_col_mads_p,x)
	}else{
		.Call(Rfast_col_mads,x)
	}
}

#[export]
mad2 <- function(x,method = "median",na.rm = FALSE) {
	.Call(Rfast_mad2,x,method,na.rm)
}