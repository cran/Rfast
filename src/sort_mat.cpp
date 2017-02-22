//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
NumericMatrix sort_mat(NumericMatrix x){
  int n=x.nrow(),p=x.ncol(),i;
  NumericMatrix::iterator start=x.begin(),end=start+n;
  for(i=0;i<p;++i){
    sort(start,end);
    start=end;
    end+=n;
  }
  return x;
}

// sort_mat
RcppExport SEXP Rfast_sort_mat(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(sort_mat(x));
    return __result;
END_RCPP
}
