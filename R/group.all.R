
group.all <- function(x, ina,ina.max = max(ina)) {
	.Call('Rfast_group_all', PACKAGE = 'Rfast',x,ina,ina.max)
}