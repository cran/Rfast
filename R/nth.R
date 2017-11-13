
nth <- function(x,k,descending=FALSE,index.return=FALSE) {
	if(index.return){
		.Call('Rfast_nth_index', PACKAGE = 'Rfast',x,k,descending)
	}else{
  		.Call('Rfast_nth', PACKAGE = 'Rfast',x,k,descending)
  	}
}