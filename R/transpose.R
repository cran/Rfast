
transpose <- function(x) {
	dm<-dim(x)
	if(dm[1]==dm[2]){
  		.Call(Rfast_transpose_sq,x)
  	}else{
  		.Call(Rfast_transpose_g_p,x)
  	}
}