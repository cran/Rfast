
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
SEXP col_any(SEXP x){
  const int n=Rf_ncols(x),p=Rf_nrows(x);
  SEXP f=Rf_allocVector(LGLSXP,n);
  int *start=LOGICAL(x),*end=start+p,*ff=LOGICAL(f);
  for(int i=0;i<n;++i,++ff){
    *ff=my_any(start,end);
    start=end;
    end+=p;
  }
  return f;
}

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

RcppExport SEXP Rfast_col_any(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_any(x);
    return __result;
END_RCPP
}
