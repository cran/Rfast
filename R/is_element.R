
is_element<-function(x,key){
  if(is.matrix(x)){
    x<-as.vector(x)
  }
  if(is.character(x)){
  	.Call(Rfast_is_element_string, x,key)
  }else{
  	.Call(Rfast_is_element, x,key)
  }
}