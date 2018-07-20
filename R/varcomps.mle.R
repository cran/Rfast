

varcomps.mle <- function(x,ina,tol=1e-09) {
	mat<-.Call(Rfast_varcomps_mle,x,ina,Rfast::sort_unique.length(ina),tol)
	syina<-mat$syina
	mat<-mat$mat
	d=mat[4]
	mat<-mat[1:3]
	ranef=mat[1]/(mat[1] + mat[2]/d)*syina/d; #mat[1]*syina/(mat[1]*d+mat[2])
	names(mat)=c("sigma_tau", "sigma_errors", "log-likelihood");
	list(info = mat, ranef = ranef);
  
}