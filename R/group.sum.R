
group.sum <- function(x, ina,ina.max = NULL,ina.min = NULL) {
	.Call(Rfast_group_sum,x,ina,ina.min,ina.max)
}