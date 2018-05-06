
nth <- function(x,k,descending=FALSE,index.return=FALSE,na.rm = FALSE) {
	if(is.integer(x)){
		.Call('Rfast_nth_int', PACKAGE = 'Rfast',x,k)
	}else if(index.return){
		.Call('Rfast_nth_index', PACKAGE = 'Rfast',x,k,descending,na.rm)
	}else{
  		.Call('Rfast_nth', PACKAGE = 'Rfast',x,k,descending,na.rm)
  	}
}