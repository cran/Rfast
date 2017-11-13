
Var <- function(x,std = FALSE,na.rm = FALSE) {
	if(na.rm){
		f <- .Call('Rfast_var_c_na_rm', PACKAGE = 'Rfast',x,which(!is.na(x)))
	}else{
		f <- .Call('Rfast_var_c', PACKAGE = 'Rfast',x)
	}
  	if(std) f <- sqrt(f)
  	f
}