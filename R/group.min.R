
group.min <- function(x, ina,ina.max = max(ina)) {
	.Call('Rfast_group_min', PACKAGE = 'Rfast',x,ina,ina.max)
}