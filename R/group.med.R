
group.med <- function(x, ina) {
	.Call('Rfast_group_med', PACKAGE = 'Rfast',x,ina)
}