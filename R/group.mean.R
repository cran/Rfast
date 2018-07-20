
group.mean <- function(x, ina,ina.max = max(ina)) {
	.Call(Rfast_group_mean,x,ina,ina.max)
}