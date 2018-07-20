
XopY.sum<-function(x,y=NULL,oper="*"){
	if(is.null(y)){
		.Call(Rfast_sum_XopX,x,oper)
	}
	else{
		.Call(Rfast_sum_XopY,x,y,oper)
	}
}