
colprods<- function(x,method = "direct"){
  .Call('Rfast_col_prods', PACKAGE = 'Rfast',x,method)
}