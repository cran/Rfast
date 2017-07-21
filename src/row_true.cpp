//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>

using namespace Rcpp;

//[[Rcpp::export]]
SEXP row_true(SEXP x){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP F=PROTECT(Rf_allocVector(INTSXP,nrow));
  int *xx=INTEGER(x),*end=xx+ncol*nrow,*f=INTEGER(F),*startx,*startf;
  const int *endf=f+LENGTH(F);
  for(startf=f;startf!=endf;++startf)
    *startf=0;
  while(xx!=end){
    for(startf=f,startx=xx,xx+=nrow;startx!=xx;++startf,++startx){
      *startf+=*startx;
    }
  }
  UNPROTECT(1);
  return F;
}

RcppExport SEXP Rfast_row_true(SEXP x){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = row_true(x);
    return __result;
END_RCPP
}
