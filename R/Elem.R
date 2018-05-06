
"Elem<-"<-function(x,value){
    if(class(x)=="iterator"){
        if(x$.method=="ceil"){
            x$.variable[x$value] <- value
        }else if(x$.method=="col"){
            x$.variable[,x$value] <- value
        }else if(x$.method=="row"){
            x$.variable[x$value,] <- value
        }else{
            stop("Error...who knows...")
        }
    }else{
        stop("Error in function 'set Elem', argument is not class 'iterator'.")
    }
    x
}

Elem<-function(x){
    if(class(x)=="iterator"){
        if(x$.method=="ceil"){
            x$.variable[x$.value]
        }else if(x$.method=="col"){
            x$.variable[,x$.value]
        }else if(x$.method=="row"){
            x$.variable[x$.value,]
        }else{
            stop("Error...who knows...")
        }
    }else{
        stop("Error in function 'get Elem', argument is not class 'iterator'.")
    }
}