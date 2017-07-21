
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

RcppExport SEXP Rfast_col_any(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_any(x);
    return __result;
END_RCPP
}
