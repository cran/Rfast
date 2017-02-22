//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
vector<double> sort_unique_double(vector<double> x){
  sort(x.begin(),x.end());
  x.erase( unique( x.begin(), x.end() ), x.end() );
  return x;
}

RcppExport SEXP Rfast_sort_unique_double(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< vector<double> >::type x(xSEXP);
    __result = wrap(sort_unique_double(x));
    return __result;
END_RCPP
}
