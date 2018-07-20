
Diag.fill <- function(x,v=0) {
	if(length(v)>1){
		.Call(Rfast_diag_fill_vec,x,v)
	}else{
		.Call(Rfast_diag_fill_scalar,x,v)
	}
}