//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using namespace std;
using namespace arma;


//[[Rcpp::export]]
SEXP col_sum_p(NumericMatrix x){
  const int n=x.ncol();
  SEXP F=PROTECT(Rf_allocVector(REALSXP,n));
  double *FF=REAL(F);
  mat xx(x.begin(),x.nrow(),n,false);
  #ifdef _OPENMP
    #pragma omp parallel for
  #endif
  for(int i=0;i<n;i++){
    FF[i]=sum(xx.col(i));
  }
  UNPROTECT(1);
  return F;
}

RcppExport SEXP Rfast_col_sum_p(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = col_sum_p(x);
    return __result;
END_RCPP
}
