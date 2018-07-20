
rowOrder <- function(x,stable=FALSE,descending=FALSE,parallel=FALSE) {
  if(parallel){
  	.Call(Rfast_row_order_p,x,stable,descending)
  }else{
  	.Call(Rfast_row_order,x,stable,descending)
  }
}