// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "mn.h"

using namespace std;
using namespace Rcpp;

double mad2(NumericVector x,const string method){
	colvec xx(x.begin(),x.size(),false);
	double res=0;
	if(method=="median"){
		const double md=med_helper(x.begin(),x.end());
		colvec y=abs(xx-md);
	  	res=med_helper(y.begin(),y.end())*center;
	}else if(method=="mean"){
		res=mean(abs(xx-mean(xx)));
	}else{
		stop("Wrong method. Choose \"median\" or \"mean\"");
	}
	return res;
}


RcppExport SEXP Rfast_mad2(SEXP xSEXP,SEXP methodSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< const string >::type method(methodSEXP);
    __result = wrap(mad2(x,method));
    return __result;
END_RCPP
}
