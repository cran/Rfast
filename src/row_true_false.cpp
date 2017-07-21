//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>

using namespace Rcpp;

//[[Rcpp::export]]
SEXP row_true_false(SEXP x){
  int ncol=Rf_ncols(x),nrow=Rf_nrows(x);
  SEXP F=PROTECT(Rf_allocMatrix(INTSXP,2,nrow));
  int *xx=INTEGER(x),*end=xx+ncol*nrow,*f=INTEGER(F),*startx,*startf;
  const int *endf=f+LENGTH(F);
  for(startf=f;startf!=endf;startf+=2){
    *startf=ncol;
    startf[1]=0;
  }
  while(xx!=end){
    for(startf=f,startx=xx,xx+=nrow;startx!=xx;startf+=2,++startx){
      *startf-=*startx;
      startf[1]+=*startx;
    }
  }
  UNPROTECT(1);
  return F;
}

RcppExport SEXP Rfast_row_true_false(SEXP x){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = row_true_false(x);
    return __result;
END_RCPP
}
