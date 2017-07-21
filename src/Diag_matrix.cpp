
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <Rinternals.h>
#include <R.h>

using namespace Rcpp;

//[[Rcpp::export]]
NumericMatrix diag_matrix(const int len,const double v){
  SEXP f=PROTECT(Rf_allocMatrix(REALSXP,len,len));
  double *ff=REAL(f),*endf=ff+len*len;
  for(;ff!=endf;++ff)
    *ff=0;
  NumericMatrix x(f);
  x.fill_diag(v);
  UNPROTECT(1);
  return x;
}

RcppExport SEXP Rfast_diag_matrix(SEXP lenSEXP,SEXP vSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const int >::type len(lenSEXP);
    traits::input_parameter< const double >::type v(vSEXP);
    __result = wrap(diag_matrix(len,v));
    return __result;
END_RCPP
}
