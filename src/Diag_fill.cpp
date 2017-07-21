
//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;

//[[Rcpp::export]]
NumericMatrix diag_fill(NumericMatrix x,const double v){
  x.fill_diag(v);
  return x;
}

RcppExport SEXP Rfast_diag_fill(SEXP xSEXP,SEXP vSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const double >::type v(vSEXP);
    __result = wrap(diag_fill(x,v));
    return __result;
END_RCPP
}
