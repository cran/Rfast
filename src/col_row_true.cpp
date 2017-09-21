//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"
#include <R.h>
#include <Rinternals.h>

using namespace Rcpp;

//[[Rcpp::export]]
SEXP col_true(SEXP x){
  const int p=Rf_nrows(x);
  SEXP f=Rf_allocVector(INTSXP,p);
  int *ff=INTEGER(f),*xx=LOGICAL(x),*endx=xx+LENGTH(x);
  for(;xx!=endx;xx+=p,++ff)
    *ff=True(xx,xx+p);
  return f;
}

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

RcppExport SEXP Rfast_col_true(SEXP x){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_true(x);
    return __result;
END_RCPP
}
