
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
double dcov(NumericMatrix x,NumericMatrix y) {
  NumericMatrix a = euclidean_dist(x,false);
  NumericMatrix b = euclidean_dist(y,false);
  const int ncla=a.ncol(),nrwa=a.nrow();
  mat aa(a.begin(),nrwa,ncla,false);
  const int nclb=b.ncol(),nrwb=b.nrow();
  mat bb(b.begin(),nrwb,nclb,false);
  rowvec ma = mean(aa,0);
  rowvec mb = mean(bb,0);
  mat A = aa.each_row() - ma;   
  A = A.each_col() - ma.t();   
  A = A + mean(ma);
  mat B = bb.each_row() - mb;  
  B = B.each_col() - mb.t();
  B = B + mean(mb);
  return sqrt(mean(mean(A % B)));
}

RcppExport SEXP Rfast_dcov(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericMatrix >::type y(ySEXP);
    __result = wrap(dcov(x,y));
    return __result;
END_RCPP
}
