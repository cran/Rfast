// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "mn.h"

using namespace std;
using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
double total_dista(NumericMatrix Xnew, NumericMatrix X) {
	const int n=X.ncol(),nu=Xnew.ncol();
	mat xnew(Xnew.begin(),Xnew.nrow(),nu,false),x(X.begin(),X.nrow(),n,false);
	double a=0.0;
	for(int i=0;i<nu;++i){
    	a+=sum_sqrt_mat(sum(square(x.each_col() - xnew.col(i)),0));
	}
  	return a;
}

RcppExport SEXP Rfast_total_dista(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericMatrix >::type y(ySEXP);
    __result = wrap(total_dista(x,y));
    return __result;
END_RCPP
}
