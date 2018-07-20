
colprods<- function(x,method = "direct"){
  .Call(Rfast_col_prods,x,method)
}