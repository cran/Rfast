//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>
#include "mn.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
vector<double> sort_cor_vecs(vector<double> x,vector<double> y){
  vector< pair<double,double> > f(x.size());
  vector< pair<double,double> >::iterator ff=f.begin();
  vector<double> F(x.size());
  vector<double>::iterator xx=x.begin(),yy=y.begin(),FF=F.begin();
  for(;xx!=x.end();++xx,++yy,++ff)
    *ff=pair<double,double>(*xx,*yy);
  sort(f.begin(),f.end(),cor_vecs);
  for(ff=f.begin();ff!=f.end();++ff,++FF)
    *FF=ff->first;
  return F;
}

RcppExport SEXP Rfast_sort_cor_vecs(SEXP xSEXP,SEXP ySEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< vector<double> >::type x(xSEXP);
    traits::input_parameter< vector<double> >::type y(ySEXP);
    __result = wrap(sort_cor_vecs(x,y));
    return __result;
END_RCPP
}
