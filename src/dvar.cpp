
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
double dvar(NumericMatrix x) {
  NumericMatrix a = euclidean_dist(x,false);
  const int ncla=a.ncol(),nrwa=a.nrow();
  mat aa(a.begin(),nrwa,ncla,false);
  rowvec ma = mean(aa,0);
  mat A = aa.each_row() - ma;   
  A = A.each_col() - ma.t();   
  A = A + mean(ma);
  return sqrt(mean(mean(square(A))));
}

RcppExport SEXP Rfast_dvar(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(dvar(x));
    return __result;
END_RCPP
}
