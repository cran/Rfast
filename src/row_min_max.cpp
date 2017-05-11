//Author: Manos Papadakis
#include <RcppArmadillo.h>
#include <Rinternals.h>
#include <R.h>


using namespace Rcpp;
using namespace arma;

  //[[Rcpp::export]]
SEXP row_min_max(SEXP X){
  NumericMatrix y(X);
  const int n=y.nrow();
  mat x(y.begin(),n,y.ncol(),false);
  colvec mn=min(x,1),mx=max(x,1);
  SEXP F=Rf_allocMatrix(REALSXP,2,n);
  double *f=REAL(F),*end=f+2*n,*mnp=&mn[0],*mxp=&mx[0];
  for(int i=0;f!=end;f+=2,++i,++mxp,++mnp){
    *f=*mnp;
    f[1]=*mxp;
  }
  return F;
}


RcppExport SEXP Rfast_row_min_max(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = row_min_max(x);
    return __result;
END_RCPP
}
