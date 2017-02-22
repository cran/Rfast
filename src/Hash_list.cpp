//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
List Hash_list(CharacterVector key,NumericVector x){
  CharacterVector::iterator k=key.begin();
  NumericVector::iterator xx=x.begin();
  List H_M;
  string m;
  for(;k!=key.end();++k,++xx){
    m=as<string>(*k);
    H_M[m]=*xx;
  }
  return H_M;
}

RcppExport SEXP Rfast_Hash_list(SEXP keySEXP,SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< CharacterVector >::type key(keySEXP);
    traits::input_parameter< NumericVector >::type x(xSEXP);
    __result = wrap(Hash_list(key,x));
    return __result;
END_RCPP
}
