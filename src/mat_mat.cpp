
//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;

IntegerVector mat_mat(NumericMatrix x,NumericMatrix y){
  const int n=x.ncol(),p=y.ncol();
  LogicalMatrix f(p,n);
  NumericVector tmp;
  for(int i=0;i<n;++i){
    tmp=x.column(i);
    for(int j=0;j<p;++j){
      f(j,i)=as<bool>(all(tmp==y.column(j)));
    }
  }
  return colSums(f);
}

RcppExport SEXP Rfast_mat_mat(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericMatrix >::type y(ySEXP);
    __result = wrap(mat_mat(x,y));
    return __result;
END_RCPP
}
