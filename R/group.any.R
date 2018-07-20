
group.any <- function(x, ina,ina.max = max(ina)) {
	.Call(Rfast_group_any,x,ina,ina.max)
}