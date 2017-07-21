//Author: Manos Papadakis

// This file was generated by compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <vector>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
vector<double> lower_tri(NumericMatrix x){
  int ncl=x.ncol(),nrw=x.nrow(),i,j;
  vector<double> f;
  if(ncl<nrw)
  	f.resize(ncl*(nrw-1)*0.5);
  else 
  	f.resize(nrw*(nrw-1)*0.5);
  vector<double>::iterator ff=f.begin();
  for(i=0;i<ncl;++i)
    for(j=i+1;j<nrw;++j,++ff)
      *ff=x(j,i);
  return f;
}

//[[Rcpp::export]]
LogicalMatrix lower_tri_b(int nrw, int ncl){
  int i,j;
  LogicalMatrix f(nrw,ncl);
  for(i=0;i<ncl;++i)
    for(j=i+1;j<nrw;++j)
      f(j,i)=true;
  return f;
}

RcppExport SEXP Rfast_lower_tri(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = wrap(lower_tri(x));
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_lower_tri_b(SEXP nclSEXP, SEXP nrwSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< int >::type ncl(nclSEXP);
    traits::input_parameter< int >::type nrw(nrwSEXP);
    __result = wrap(lower_tri_b(nrw,ncl));
    return __result;
END_RCPP
}
