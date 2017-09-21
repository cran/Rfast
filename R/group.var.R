
group.var <- function(x, ina,ina.max = max(ina)) {
	.Call('Rfast_group_var', PACKAGE = 'Rfast',x,ina,ina.max)
}