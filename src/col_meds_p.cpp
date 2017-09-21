//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include <algorithm>

#ifdef _OPENMP
#include <omp.h>
#endif

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
SEXP col_meds_p(NumericMatrix x){
  const int p=x.ncol(),step=x.nrow(),middle=step/2-1;
  mat xx(x.begin(),step,p,false);
  SEXP F=PROTECT(Rf_allocVector(REALSXP,p));
  double *FF=REAL(F);
  if(step%2==0){
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<p;++i){
      colvec tmp=xx.col(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      FF[i]=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0;
    }
  }else{
    #ifdef _OPENMP
    #pragma omp parallel for
    #endif
    for(int i=0;i<p;++i){
      colvec tmp=xx.col(i);
      nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
      FF[i]=tmp[middle+1];
    }
  }
  UNPROTECT(1);
  return F;
}

// colMedians
RcppExport SEXP Rfast_col_meds_p(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = col_meds_p(x);
    return __result;
END_RCPP
}
