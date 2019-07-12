
group.max <- function(x, ina,ina.min = NULL,ina.max = NULL){
	.Deprecated("Rfast::group")
	.Call(Rfast_group_max,x,ina,ina.min,ina.max)
}