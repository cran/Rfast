//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>
#include <vector>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
bool binarysearch(vector<double> x,double v){
	return binary_search(x.begin(),x.end(),v);
}

//[[Rcpp::export]]
int lowerbound(vector<double> x,double v){
	return lower_bound(x.begin(),x.end(),v)-x.begin()+1;
}

RcppExport SEXP Rfast_binarysearch(SEXP xSEXP,SEXP vSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< vector<double> >::type x(xSEXP);
    traits::input_parameter< double >::type v(vSEXP);
    __result = wrap(binarysearch(x,v));
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_lowerbound(SEXP xSEXP,SEXP vSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< vector<double> >::type x(xSEXP);
    traits::input_parameter< double >::type v(vSEXP);
    __result = wrap(lowerbound(x,v));
    return __result;
END_RCPP
}
