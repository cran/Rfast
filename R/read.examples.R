

read.examples<-function(path.man,dont.read = ""){
  .Call("Rfast_read_examples",PACKAGE = "Rfast",path.man,dont.read)
}