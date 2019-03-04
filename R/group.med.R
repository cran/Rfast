
group.med <- function(x,ina,ina.length.unique=length(unique(ina))) {
	.Call(Rfast_group_med,x,ina,ina.length.unique)
}