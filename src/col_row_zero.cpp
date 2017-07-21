// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;

//[[Rcpp::export]]
bool col_row_zero(NumericMatrix X){
  int i,n=X.nrow(),p=X.ncol();
  mat x(X.begin(),n,p,false);
  for(i=0;i<p;++i){
    if(all(x.col(i)==0.0)){
      return true;
    }
  }
  for(i=0;i<n;++i){
    if(all(x.row(i)==0.0)){
      return true;
    }
  }
  return false;
}

RcppExport SEXP Rfast_col_row_zero(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(col_row_zero(x));
    return __result;
END_RCPP
}
