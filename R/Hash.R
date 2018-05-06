
Hash<-function(keys=NULL,values=NULL){
	keys <- if(is.character(keys)) keys else as.character(keys)
	i <- 1
	len <- length(keys)
	x <- new.env(size=len)
	x$.length <- len
	for(key in keys){
		x[[key]] <- values[i]
		i <- i + 1
	}
	class(x)<-"Hash"
	x
}