
Var <- function(x,std = FALSE,na.rm = FALSE) {
	if(na.rm){
		f <- .Call(Rfast_var_c_na_rm,x,which(!is.na(x)))
	}else{
		f <- .Call(Rfast_var_c,x)
	}
  	if(std) f <- sqrt(f)
  	f
}