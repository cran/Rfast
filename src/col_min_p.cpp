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
NumericVector col_min_p(NumericMatrix x){
  const int n=x.ncol();
  mat xx(x.begin(),x.nrow(),n,false);
  NumericVector f(n);
  #ifdef _OPENMP
    #pragma omp parallel for
  #endif
  for(int i=0;i<n;++i){
    f[i]=*min_element(xx.begin_col(i),xx.end_col(i));
  }
  return f;
}

RcppExport SEXP Rfast_col_min_p(SEXP nSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type n(nSEXP);
    __result = wrap(col_min_p(n));
    return __result;
END_RCPP
}
