//Author: Manos Papadakis

#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
double hash_find(List x,string value){
  vector<string> nam=x.names();
  if(find(nam.begin(),nam.end(),value)!=nam.end())
    return as<double>(x[value]);
  return 0.0;
}

RcppExport SEXP Rfast_hash_find(SEXP xSEXP,SEXP valueSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< List >::type x(xSEXP);
    traits::input_parameter< string >::type value(valueSEXP);
    __result = wrap(hash_find(x,value));
    return __result;
END_RCPP
}
