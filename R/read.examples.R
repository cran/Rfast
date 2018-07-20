

read.examples<-function(path.man,dont.read = ""){
  .Call(Rfast_read_examples,path.man,dont.read)
}