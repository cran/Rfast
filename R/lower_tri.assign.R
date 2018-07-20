

lower_tri.assign <- function(x,v,diag = FALSE) {
	.Call(Rfast_lower_tri_assign,x,v,diag)
}