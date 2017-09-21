
group.sum <- function(x, ina,ina.max = max(ina)) {
	.Call('Rfast_group_sum', PACKAGE = 'Rfast',x,ina,ina.max)
}