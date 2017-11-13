//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "templates.h"

using namespace Rcpp;

//[[Rcpp::export]]
IntegerVector col_count_values(NumericMatrix x,NumericVector values){
  const int n=values.size();
  IntegerVector f(n);
  for(int i=0;i<n;++i){
  	f[i]=count_value_helper<NumericVector,double,double*>(x.column(i),values[i]);
  }
  return f;
}

RcppExport SEXP Rfast_col_count_values(SEXP xSEXP,SEXP valuesSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type values(valuesSEXP);
    __result = wrap(col_count_values(x,values));
    return __result;
END_RCPP
}


//[[Rcpp::export]]
IntegerVector row_count_values(NumericMatrix x,NumericVector values){
  const int n=values.size();
  IntegerVector f(n);
  for(int i=0;i<n;++i){
  	f[i]=count_value_helper<NumericVector,double,double*>(x.row(i),values[i]);
  }
  return f;
}

RcppExport SEXP Rfast_row_count_values(SEXP xSEXP,SEXP valuesSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericVector >::type values(valuesSEXP);
    __result = wrap(row_count_values(x,values));
    return __result;
END_RCPP
}
