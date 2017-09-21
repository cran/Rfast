
group.any <- function(x, ina,ina.max = max(ina)) {
	.Call('Rfast_group_any', PACKAGE = 'Rfast',x,ina,ina.max)
}