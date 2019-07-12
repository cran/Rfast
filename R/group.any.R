
group.any <- function(x, ina,ina.max = max(ina)) {
	.Deprecated("Rfast::group")
	.Call(Rfast_group_any,x,ina,ina.max)
}