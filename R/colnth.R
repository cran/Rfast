
colnth <- function(x,elems,descending=FALSE,na.rm = FALSE,index.return = FALSE,parallel = FALSE) {
	if(parallel){
  		.Call(Rfast_col_nth_p,x,elems,descending,na.rm,index.return)
	}else{
		.Call(Rfast_col_nth,x,elems,descending,na.rm,index.return)
	}
}