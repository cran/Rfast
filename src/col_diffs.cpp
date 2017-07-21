//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
SEXP col_diffs(SEXP x){
  const int n=Rf_nrows(x),p=Rf_ncols(x);
  SEXP f=Rf_allocMatrix(REALSXP,n,p-1);
  double *ff=REAL(f),*xx=REAL(x),*l=xx+n,*end=ff+Rf_length(f);
  for(;ff!=end;++ff,++l,++xx)
    *ff=*l-*xx;
  return f;
}

RcppExport SEXP Rfast_col_diffs(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_diffs(x);
    return __result;
END_RCPP
}
