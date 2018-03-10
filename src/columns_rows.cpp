
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>
#include <string>
#include "templates.h"

using namespace Rcpp;

NumericMatrix columns(NumericMatrix x,IntegerVector ind){
  const int nrw=x.nrow(),ncl=ind.size();
  NumericMatrix f(nrw,ncl);
  mat ff(f.begin(),nrw,ncl,false),xx(x.begin(),nrw,x.ncol(),false);
  for(int i=0;i<ncl;++i)
    ff.col(i)=xx.col(ind[i]-1);
  return f;
}

SEXP rows(SEXP X,SEXP Ind){
  const int nrw=Rf_nrows(X),ncl=Rf_ncols(X);
  SEXP F=PROTECT(Rf_allocMatrix(REALSXP,LENGTH(Ind),ncl));
  double *start = REAL(X),*ff=REAL(F),*xx=start;
  int *start_ind=INTEGER(Ind),*ind,*end_ind=start_ind+LENGTH(Ind);
  for(int i=0;i<ncl;++i){
    for(ind=start_ind;ind!=end_ind;++ind){
      xx=start+ *ind-1;
      *ff++=*xx;
    }
    start+=nrw;
  }
  UNPROTECT(1);
  return F;
}

RcppExport SEXP Rfast_columns(SEXP xSEXP,SEXP indSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type ind(indSEXP);
    __result = wrap(columns(x,ind));
    return __result;
END_RCPP
}


RcppExport SEXP Rfast_rows(SEXP x,SEXP ind) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = rows(x,ind);
    return __result;
END_RCPP
}
