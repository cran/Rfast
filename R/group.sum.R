
group.sum <- function(x, ina) {
	.Call('Rfast_group_sum', PACKAGE = 'Rfast',x,ina)
}