//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
double nth(vector<double> x,unsigned int elem){
  nth_element(x.begin(),x.begin()+elem-1,x.end());
  return x[elem-1];
}

// nth_element
RcppExport SEXP Rfast_nth(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< vector<double> >::type x(xSEXP);
    traits::input_parameter< unsigned int >::type y(ySEXP);
    __result = wrap(nth(x,y));
    return __result;
END_RCPP
}
