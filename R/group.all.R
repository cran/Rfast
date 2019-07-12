
group.all <- function(x, ina,ina.max = max(ina)) {
	.Deprecated("Rfast::group")
	.Call(Rfast_group_all,x,ina,ina.max)
}