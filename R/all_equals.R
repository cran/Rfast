all_equals<-function(x,y){
	if(class(x)!=class(y)){
		stop("Different types")
	}
	all(identical(round(x,16),round(y,16)))
}