//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
IntegerVector col_len_sort_un_int(IntegerMatrix x){
  const int p=x.ncol();
  IntegerVector f(p);
  for(int i=0;i<p;++i)
    f[i]=len_sort_unique_int(x.column(i));
  return f;
}

RcppExport SEXP Rfast_col_len_sort_un_int(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< IntegerMatrix >::type x(xSEXP);
    __result = wrap(col_len_sort_un_int(x));
    return __result;
END_RCPP
}
