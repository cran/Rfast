//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>
#include "mn.h"

using namespace std;

//[[Rcpp::export]]
vector<double> floyd_john(int n,vector<double> x){
  i4mat_floyd(n,x);
  return x;
}

RcppExport SEXP Rfast_floyd_john(SEXP nSEXP,SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< int >::type n(nSEXP);
    traits::input_parameter< vector<double> >::type x(xSEXP);
    __result = wrap(floyd_john(n,x));
    return __result;
END_RCPP
}
