//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;

//[[Rcpp::export]]
SEXP col_false(SEXP x){
  const int p=Rf_ncols(x),n=Rf_nrows(x);
  SEXP f=Rf_allocVector(INTSXP,p);
  int *ff=INTEGER(f),*xx=LOGICAL(x),*endx=xx+LENGTH(x);
  for(;xx!=endx;xx+=p,++ff)
    *ff=n-True(xx,xx+p);
  return f;
}

RcppExport SEXP Rfast_col_false(SEXP x){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_false(x);
    return __result;
END_RCPP
}
