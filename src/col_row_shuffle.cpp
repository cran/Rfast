//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <random>
#include <chrono>

using namespace Rcpp;
using namespace arma;

//[[Rcpp::export]]
IntegerVector col_shuffle(const int len,const int n){
  IntegerVector ind=seq_len(n*len);
  IntegerVector::iterator start=ind.begin(),end=start+len;
  unsigned seed = std::chrono::system_clock::now().time_since_epoch().count();
  for(int i=0;i<n;++i,start=end,end+=len){
  	std::shuffle(start,end,std::default_random_engine(seed));
  }
  return ind;
}

//[[Rcpp::export]]
mat row_shuffle(mat x){
  return shuffle(x,1);
}

RcppExport SEXP Rfast_row_shuffle(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< mat >::type x(xSEXP);
    __result = row_shuffle(x);
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_col_shuffle(SEXP lenSEXP,SEXP nSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const int >::type len(lenSEXP);
    traits::input_parameter< const int >::type n(nSEXP);
    __result = col_shuffle(len,n);
    return __result;
END_RCPP
}
