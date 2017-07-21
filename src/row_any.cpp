
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
SEXP row_any(SEXP x){
  int nrow=Rf_nrows(x);
  SEXP F=PROTECT(Rf_allocVector(LGLSXP,nrow));
  int *xx=INTEGER(x),*endx=xx+LENGTH(x),*f=INTEGER(F),*startx,*startf;
  const int *endf=f+LENGTH(F);
  for(startf=f;startf!=endf;++startf)
    *startf=0;
  while(xx!=endx){
    for(startf=f,startx=xx,xx+=nrow;startx!=xx;++startf,++startx){
      if(*startx){
        *startf=1;
      }
    }
  }
  UNPROTECT(1);
  return F;
}

RcppExport SEXP Rfast_row_any(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = row_any(x);
    return __result;
END_RCPP
}
