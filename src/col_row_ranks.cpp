
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;
using namespace std;


//[[Rcpp::export]]
NumericMatrix col_ranks(NumericMatrix x,string method,const bool descend,const bool stable){
  const int n=x.ncol();
  NumericMatrix f(x.nrow(),n);
  for(int i=0;i<n;++i){
    f.column(i)=Rank(x.column(i),method,descend,stable);
  }
  return f; 
}

RcppExport SEXP Rfast_col_ranks(SEXP xSEXP,SEXP methodSEXP,SEXP descendSEXP,SEXP stableSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< string >::type method(methodSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);    
    __result = wrap(col_ranks(x,method,descend,stable));
    return __result;
END_RCPP
}

//[[Rcpp::export]]
NumericMatrix row_ranks(NumericMatrix x,string method,const bool descend,const bool stable){
  const int n=x.nrow();
  NumericMatrix f(n,x.ncol());
  for(int i=0;i<n;++i){
    f.row(i)=Rank(x.row(i),method,descend,stable);
  }
  return f; 
}

RcppExport SEXP Rfast_row_ranks(SEXP xSEXP,SEXP methodSEXP,SEXP descendSEXP,SEXP stableSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< string >::type method(methodSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    traits::input_parameter< const bool >::type stable(stableSEXP);
    __result = wrap(row_ranks(x,method,descend,stable));
    return __result;
END_RCPP
}
