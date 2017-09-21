//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
SEXP col_nth_p(NumericMatrix x,IntegerVector elems){
  const int n=elems.size();
  mat xx(x.begin(),x.nrow(),n,false);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,n));
  double *FF=REAL(F);
  #ifdef _OPENMP
    #pragma omp parallel for
  #endif
  for(int i=0;i<n;++i){
    colvec y=xx.col(i);
    int elem=elems[i]-1;
    nth_element(y.begin(),y.begin() + elem,y.end());
    FF[i]=y[elem];
  }
  UNPROTECT(1);
  return F;
}

// nth_element
RcppExport SEXP Rfast_col_nth_p(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type y(ySEXP);
    __result = wrap(col_nth_p(x,y));
    return __result;
END_RCPP
}

//[[Rcpp::export]]
SEXP row_nth_p(NumericMatrix x,IntegerVector elems){
  const int n=elems.size();
  mat xx(x.begin(),n,x.ncol(),false);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,n));
  double *FF=REAL(F);
  #ifdef _OPENMP
    #pragma omp parallel for
  #endif
  for(int i=0;i<n;++i){
    rowvec y=xx.row(i);
    int elem=elems[i]-1;
    nth_element(y.begin(),y.begin() + elem,y.end());
    FF[i]=y[elem];
  }
  UNPROTECT(1);
  return F;
}

RcppExport SEXP Rfast_row_nth_p(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< IntegerVector >::type y(ySEXP);
    __result = row_nth_p(x,y);
    return __result;
END_RCPP
}
