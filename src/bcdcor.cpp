
//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include "mn.h"

using namespace Rcpp;
using namespace arma;
using namespace std;


//[[Rcpp::export]]
vec lower_tri(mat &x){
  int n=x.n_cols,i,j;
  vec f(n*(n-1)*0.5);
  vec::iterator ff=f.begin();
  for(i=0;i<n;++i)
    for(j=i+1;j<n;++j,++ff)
      *ff=x(j,i);
  return f;
}


//[[Rcpp::export]]
double bcdcor(NumericMatrix x,NumericMatrix y) {
  NumericMatrix a = euclidean_dist(x,false);
  NumericMatrix b = euclidean_dist(y,false);
  const double n=a.ncol();
  mat aa(a.begin(),n,n,false);
  mat bb(b.begin(),n,n,false);
  rowvec ma = mean(aa,0),dgA(n);
  rowvec mb = mean(bb,0),dgB(n);
  const double mean_ma=mean(ma),mean_mb=mean(mb);
  double n_1=n/(n-1),n_2=n/(n-2);
  dgA=ma-mean_ma;
  dgB=mb-mean_mb;
  mat A = aa.each_row() - ma;   
  A = A.each_col() - ma.t();   
  A = A + mean_ma - aa/n;
  vec Al=lower_tri(A);
  mat B = bb.each_row() - mb;  
  B = B.each_col() - mb.t();
  B = B + mean_mb - bb/n;
  vec Bl=lower_tri(B);
  n_1*=n_1;
  n_2*=n_1;
  const double sdgab=sum(dgA%dgB),sdga=sum(square(dgA)),sdgb=sum(square(dgB));
  const double XY = n_1*(sum(Al%Bl)*2+sdgab) - n_2 * sdgab;
  const double XX = n_1*(sum(square(Al))*2+sdga) - n_2 * sdga;
  const double YY = n_1*(sum(square(Bl))*2+sdgb) - n_2 * sdgb;
  return XY / sqrt(XX * YY);
}

RcppExport SEXP Rfast_bcdcor(SEXP xSEXP,SEXP ySEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    traits::input_parameter< NumericMatrix >::type y(ySEXP);
    __result = wrap(bcdcor(x,y));
    return __result;
END_RCPP
}
