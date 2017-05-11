//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>
#include <string>
#include "mn.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
vector<string> sort_string(CharacterVector x,const bool descend){
  vector<string> f(x.begin(),x.end());
  if(descend)
  	sort(f.begin(),f.end(),descending_string);
  else
  	sort(f.begin(),f.end());
  return f;
}

RcppExport SEXP Rfast_sort_string(SEXP xSEXP,SEXP descendSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< CharacterVector >::type x(xSEXP);
    traits::input_parameter< const bool >::type descend(descendSEXP);
    __result = wrap(sort_string(x,descend));
    return __result;
END_RCPP
}
