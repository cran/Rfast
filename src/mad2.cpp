// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "templates.h"

using std::remove_if;
using std::string;
using namespace Rcpp;
using namespace arma;

double mad2(NumericVector X,const string method,const bool na_rm){
  const double center = 1.4826;
	NumericVector xx=clone(X);
	const int newsize = na_rm ? remove_if(xx.begin(),xx.end(),R_IsNA)-xx.begin() : xx.size();
	colvec x(xx.begin(),newsize,false);
	double res=0;
	if(method=="median"){
		const double md=med_helper<colvec>(x.begin(),x.end());
		colvec y=abs(x-md);
	  	res=med_helper<colvec>(y.begin(),y.end())*center;
	}else if(method=="mean"){
		res=mean(abs(x-mean(x)));
	}else{
		stop("Wrong method. Choose \"median\" or \"mean\"");
	}
	return res;
}


RcppExport SEXP Rfast_mad2(SEXP xSEXP,SEXP methodSEXP,SEXP na_rmSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< const string >::type method(methodSEXP);
    traits::input_parameter< const bool >::type na_rm(na_rmSEXP);
    __result = wrap(mad2(x,method,na_rm));
    return __result;
END_RCPP
}
