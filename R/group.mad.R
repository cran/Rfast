
group.mad <- function(x,ina,method = "median") {
	.Call('Rfast_group_mad', PACKAGE = 'Rfast',x,ina,method)
}