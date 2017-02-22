
data.frame.to_matrix <- function(x) {
	.Call('Rfast_frame_to_matrix', PACKAGE = 'Rfast',x,as.numeric)
}