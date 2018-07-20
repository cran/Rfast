
group.min_max <- function(x, ina,ina.max = max(ina)) {
	.Call(Rfast_group_min_max,x,ina,ina.max)
}