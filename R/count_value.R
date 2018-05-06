
count_value <- function(x,value) {
	.Call(Rfast_count_value,x,value)
}