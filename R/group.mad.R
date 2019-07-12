
group.mad <- function(x,ina,method = "median") {
	.Deprecated("Rfast::group")
	.Call(Rfast_group_mad,x,ina,method)
}