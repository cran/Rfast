
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

//[[Rcpp::export]]
List dcor(NumericMatrix x,NumericMatrix y) {
  NumericMatrix a = euclidean_dist(x,false);
  NumericMatrix b = euclidean_dist(y,false);
  const int n=a.ncol();
  mat aa(a.begin(),n,n,false);
  mat bb(b.begin(),n,n,false);
  rowvec ma = mean(aa,0);
  rowvec mb = mean(bb,0);
  mat A = aa.each_row() - ma;   
  A = A.each_col() - ma.t();   
  A = A + mean(ma);
  mat B = bb.each_row() - mb;  
  B = B.each_col() - mb.t();
  B = B + mean(mb);
  const double dcov = sqrt( mean(mean(A % B)) );
  const double dvarX = sqrt( mean(mean(square(A))) );
  const double dvarY = sqrt( mean(mean(square(B))) );
  const double dcor = dcov/sqrt(dvarX * dvarY);
  List l;
  l["dcov"] = dcov;
  l["dvarX"] = dvarX;
  l["dvarY"] = dvarY;
  l["dcor"] = dcor;
  return l;
}

RcppExport SEXP Rfast_dcor(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericMatrix >::type y(ySEXP);
    __result = wrap(dcor(x,y));
    return __result;
END_RCPP
}
