all_equals<-function(x,y){
	if(class(x)!=class(y)){
		stop("Different types")
	}
	all(identical(x,y))
}