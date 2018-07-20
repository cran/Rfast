
poisson_only <- function(x,y,tol = 1e-09,b_values=FALSE) {
	if(b_values){
		x<-.Call(Rfast_poisson_only_b,x,y,sum(y*log(y),na.rm=TRUE),tol)
		rownames(x)<-c("log-lik","constant","slope")
		return (x)
	}
	.Call(Rfast_poisson_only,x,y,sum(y*log(y),na.rm=TRUE),tol)
}