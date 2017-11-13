
group.sum <- function(x, ina,ina.max = NULL,ina.min = NULL) {
	.Call('Rfast_group_sum', PACKAGE = 'Rfast',x,ina,ina.min,ina.max)
}