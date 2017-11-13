//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "templates.h"

#ifdef _OPENMP
#include <omp.h>
#endif


using namespace Rcpp;
using namespace arma;


//[[Rcpp::export]]
IntegerVector col_count_values_p(NumericMatrix x,NumericVector values){
  const int n=values.size(),p=x.nrow();
  IntegerVector f(n);
  mat xx(x.begin(),p,n,false);
  ivec ff(f.begin(),n,false);
  colvec vv(values.begin(),n,false);
  #ifdef _OPENMP
    #pragma omp parallel for
  #endif
  for(int i=0;i<n;++i){
  	ff[i]=count_value_helper<colvec,double,double*>(xx.col(i),vv[i]);
  }
  return f;
}

RcppExport SEXP Rfast_col_count_values_p(SEXP xSEXP,SEXP valuesSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type values(valuesSEXP);
    __result = wrap(col_count_values_p(x,values));
    return __result;
END_RCPP
}


//[[Rcpp::export]]
IntegerVector row_count_values_p(NumericMatrix x,NumericVector values){
  const int n=values.size(),p=x.nrow();
  IntegerVector f(n);
  mat xx(x.begin(),p,n,false);
  ivec ff(f.begin(),n,false);
  colvec vv(values.begin(),n,false);
  #ifdef _OPENMP
    #pragma omp parallel for
  #endif
  for(int i=0;i<n;++i){
  	ff[i]=count_value_helper<rowvec,double,double*>(xx.row(i),vv[i]);
  }
  return f;
}

RcppExport SEXP Rfast_row_count_values_p(SEXP xSEXP,SEXP valuesSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type values(valuesSEXP);
    __result = wrap(row_count_values_p(x,values));
    return __result;
END_RCPP
}
