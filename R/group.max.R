
group.max <- function(x, ina,ina.max = max(ina)) {
	.Call('Rfast_group_max', PACKAGE = 'Rfast',x,ina,ina.max)
}