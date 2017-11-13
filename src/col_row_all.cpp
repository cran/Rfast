
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
SEXP col_all(SEXP x){
  const int n=Rf_ncols(x),p=Rf_nrows(x);
  SEXP f=Rf_allocVector(LGLSXP,n);
  int *start=LOGICAL(x),*end=start+p,*ff=LOGICAL(f);
  for(int i=0;i<n;++i,++ff){
    *ff=my_all(start,end);
    start=end;
    end+=p;
  }
  return f;
}

RcppExport SEXP Rfast_col_all(SEXP x) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = col_all(x);
    return __result;
END_RCPP
}

//[[Rcpp::export]]
LogicalVector row_all(LogicalMatrix x){
  const int n=x.nrow();
  LogicalVector f(n);
  for(int i=0;i<n;++i)
    f[i]=as<bool>(all(x.row(i)));
  return f;
}

RcppExport SEXP Rfast_row_all(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< LogicalMatrix >::type x(xSEXP);
    __result = wrap(row_all(x));
    return __result;
END_RCPP
}
