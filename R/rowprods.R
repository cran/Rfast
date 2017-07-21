

rowprods<- function(x){
  .Call('Rfast_row_prods',PACKAGE = "Rfast",x)
}