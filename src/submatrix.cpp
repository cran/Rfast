//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;

NumericMatrix submatrix(NumericMatrix x,const int rowstart,const int rowend,const int colstart,const int colend){
  return x(Range(rowstart-1,rowend-1),Range(colstart-1,colend-1));
}

RcppExport SEXP Rfast_submatrix(SEXP xSEXP,SEXP rowstartSEXP,SEXP rowendSEXP,SEXP colstartSEXP,SEXP colendSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const int >::type rowstart(rowstartSEXP);
    traits::input_parameter< const int >::type rowend(rowendSEXP);
    traits::input_parameter< const int >::type colstart(colstartSEXP);
    traits::input_parameter< const int >::type colend(colendSEXP);
    __result = wrap(submatrix(x,rowstart,rowend,colstart,colend));
    return __result;
END_RCPP
}
