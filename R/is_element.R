
is_element<-function(x,key){
  if(is.matrix(x))
    x<-as.vector(x)
  .Call('Rfast_is_element', PACKAGE = 'Rfast', x,key)
}