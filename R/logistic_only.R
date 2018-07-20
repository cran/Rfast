
logistic_only <- function(x,y,tol = 1e-09,b_values = FALSE) {
	if(b_values){
		x<-.Call(Rfast_logistic_only_b,x,y,tol)
		rownames(x)<-c("log-lik","constant","slope")
		return (x)
	}
	.Call(Rfast_logistic_only,x,y,tol)
}