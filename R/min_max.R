min_max<-function(x,index=FALSE){
	.Call('Rfast_min_max',x,index)
}