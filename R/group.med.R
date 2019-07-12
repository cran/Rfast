
group.med <- function(x, ina,ina.length.unique=length(unique(ina))) {
	.Deprecated("Rfast::group")
	.Call(Rfast_group_med,x,ina,ina.length.unique)
}