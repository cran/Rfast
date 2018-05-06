
Hash.key.multi<-function(x,...,sep = " "){
    x<-.Call(Rfast_Hash_key_multi,x,paste(...,collapse = sep),sep)
    if(x == "") NULL else x
}