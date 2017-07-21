
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>

using namespace Rcpp;

//[[Rcpp::export]]
SEXP pmin(SEXP x,SEXP y){
  SEXP f=Rf_allocVector(REALSXP,LENGTH(x));
  double *startx=REAL(x),*end=startx+LENGTH(x),*starty=REAL(y),*startf=REAL(f);
  for(;startx!=end;++startx,++starty,++startf)
    *startf=std::min(*startx,*starty);
  return f;
}

RcppExport SEXP Rfast_pmin(SEXP x,SEXP y) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = pmin(x,y);
    return __result;
END_RCPP
}
