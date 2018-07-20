
group.var <- function(x, ina,ina.max = max(ina)) {
	.Call(Rfast_group_var,x,ina,ina.max)
}