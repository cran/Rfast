
group.min <- function(x, ina,ina.max = max(ina)) {
	.Deprecated("Rfast::group")
	.Call(Rfast_group_min,x,ina,ina.max)
}