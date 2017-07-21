
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>

using namespace Rcpp;

//[[Rcpp::export]]
SEXP pmax(SEXP x,SEXP y){
  SEXP f=Rf_allocVector(REALSXP,LENGTH(x));
  double *startx=REAL(x),*end=startx+LENGTH(x),*starty=REAL(y),*startf=REAL(f);
  for(;startx!=end;++startx,++starty,++startf)
    *startf=std::max(*startx,*starty);
  return f;
}

RcppExport SEXP Rfast_pmax(SEXP x,SEXP y) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = pmax(x,y);
    return __result;
END_RCPP
}
