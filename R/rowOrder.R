
rowOrder <- function(x,stable=FALSE,descending=FALSE,parallel=FALSE) {
  if(parallel){
  	.Call('Rfast_row_order_p', PACKAGE = 'Rfast',x,stable,descending)
  }else{
  	.Call('Rfast_row_order', PACKAGE = 'Rfast',x,stable,descending)
  }
}