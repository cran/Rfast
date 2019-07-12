
group.var <- function(x, ina,ina.max = max(ina)) {
	.Deprecated("Rfast::group")
	.Call(Rfast_group_var,x,ina,ina.max)
}