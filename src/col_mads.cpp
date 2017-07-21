
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
mat col_mads(NumericMatrix x){
  mat xx(x.begin(),x.nrow(),x.ncol(),false);
  rowvec m = median(xx,0);
  mat y = xx.each_row() - m;
  return median( abs(y),0 ) * 1.482602;
}

RcppExport SEXP Rfast_col_mads(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(col_mads(x));
    return __result;
END_RCPP
}
