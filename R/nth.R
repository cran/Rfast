
nth <- function(x,k,descending=FALSE,index.return=FALSE,na.rm = FALSE) {
	if(is.integer(x)){
		.Call(Rfast_nth_int,x,k)
	}else{
  		.Call(Rfast_nth,x,k,descending,na.rm,index.return)
  	}
}