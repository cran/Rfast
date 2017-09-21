
Order<-function(x,stable=FALSE,descending=FALSE){
	if(is.character(x)){
		x <- as.numeric(x)
	}
	.Call("Rfast_Order",x,stable,descending)
}