
//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
mat row_mads(NumericMatrix x){
  mat xx(x.begin(),x.nrow(),x.ncol(),false);
  rowvec m = median(xx,1);
  mat y = xx.each_col() - m;
  return median( abs(y),1 ) * 1.482602;
}

RcppExport SEXP Rfast_row_mads(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(row_mads(x));
    return __result;
END_RCPP
}
