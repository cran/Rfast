// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include "mn.h"

using namespace std;
using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
double edist(NumericMatrix x,NumericMatrix y){
	const int n1=x.ncol(),n2=y.ncol();
	double mij=total_dista(x, y),mii=total_dist(x),mjj=total_dist(y);
	return (2 * mij - n2 * mii / n1 - n1 * mjj/n2 ) / (n1 + n2);
}

RcppExport SEXP Rfast_edist(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericMatrix >::type y(ySEXP);
    __result = wrap(edist(x,y));
    return __result;
END_RCPP
}
