
colOrder <- function(x,stable=FALSE,descending=FALSE,parallel=FALSE) {
  if(parallel){
  	.Call(Rfast_col_order_p,x,stable,descending)
  }else{
  	.Call(Rfast_col_order,x,stable,descending)
  }
}