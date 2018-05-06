
Stack<-function(x,type=NULL){
    y<-new.env()
    class(y)<-"Stack"
    var<- if(class(x)=="Stack") y$.variable else x
    y$.variable<-if(is.null(type)){ y$.total<-length(var);var }else{ y$.total<-x;type(x)}
    y$.top_element<-y$.total
    y$pop = function(){ 
        if(y$.top_element>0){
            tmp <- y$.variable[y$.top_element]
            y$.top_element<-y$.top_element-1
            tmp
        }else{
            stop("Stack is empty. You have to fill it first.")
        }
    }
    y$top = function(){
        if(y$.top_element>0){
            y$.variable[y$.top_element]
        }else{
            stop("Stack is empty. You have to fill it first.")
        }
    }
    y$push = function(x){
        if(y$.top_element==y$.total){
            y$.top_element=y$.top_element+1
            y$.variable=c(y$.variable,x)
        }else{
            y$.top_element=y$.top_element+1
            y$.variable[y$.top_element]=x
        }
    }
    lockEnvironment(y)
    y
}

#Stack.env2<-function(x){
#    y<-new.env()
#    class(y)<-"Stack"
#    var<- if(class(x)=="Stack") y$.variable else x
#    y$.variable<-var
#    y$.total<-length(x)
#    y$.top_element<-y$.total
#    lockEnvironment(y
#    y
#}
#
#pop = function(){ 
#    if(y$.top_element>0)
#        tmp <- y$.variable[y$.top_element]
#        y$.top_element<-y$.top_element-
#        tmp
#    }else{
#        stop("Stack is empty. You have to fill it first.")
#    }
#}
#top = function(y){
#    if(y$.top_element>0){
#        y$.variable[y$.top_element]
#    }else{
#        stop("Stack is empty. You have to fill it first.")
#    }
#}
#
#push = function(y,x)
#    if(y$.top_element==y$.total){
#        y$.top_element=y$.top_element+1
#        y$.variable=c(y$.variable,x)
#    }else{
#        y$.top_element=y$.top_element+1
#        y$.variable[y$.top_element]=x
#    
#}
