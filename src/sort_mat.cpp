//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>
#include "mn.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
NumericMatrix sort_col(NumericMatrix x,const bool descend){
  const int n=x.nrow(),p=x.ncol();
  NumericVector coli(n);
  NumericMatrix f(n,p);
  if(descend)  
    for(int i=0;i<p;++i){
      coli=x.column(i);
      sort(coli.begin(),coli.end(),descending_double);
      f.column(i)=coli;
    }
  else
    for(int i=0;i<p;++i){
      coli=x.column(i);
      sort(coli.begin(),coli.end());
      f.column(i)=coli;
    }  
  return f;
}

//[[Rcpp::export]]
NumericMatrix sort_row(NumericMatrix x,const bool descend){
  const int sz=x.ncol(),p=x.nrow();
  NumericVector rowi(sz);
  if(descend)
    for(int i=0;i<p;++i){
      rowi=x.row(i);
      sort(rowi.begin(),rowi.end(),descending_double);
      x.row(i)=rowi;
    }
  else
    for(int i=0;i<p;++i){
      rowi=x.row(i);
      sort(rowi.begin(),rowi.end());
      x.row(i)=rowi;
    } 
  return x;
}

// sort_col
RcppExport SEXP Rfast_sort_col(SEXP xSEXP,SEXP descendSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    __result = wrap(sort_col(x,descend));
    return __result;
END_RCPP
}

// sort_row
RcppExport SEXP Rfast_sort_row(SEXP xSEXP,SEXP descendSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    __result = wrap(sort_row(x,descend));
    return __result;
END_RCPP
}
