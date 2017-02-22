//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>
#include "mn.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
vector<int> sort_index(vector<double> x,bool descending){
  vector< pair<int,double> > f(x.size());
  vector< pair<int,double> >::iterator ff=f.begin();
  vector<int> F(x.size());
  vector<int>::iterator FF=F.begin();
  vector<double>::iterator xx=x.begin();
  for(int i=0;xx!=x.end();++xx,++i,++ff)
    *ff=pair<int,double>(i,*xx);
  if(descending)
    sort(f.begin(),f.end(),s_indx_des);
  else
    sort(f.begin(),f.end(),s_indx_asc);
  for(ff=f.begin();ff!=f.end();++ff,++FF)
    *FF=ff->first;
  return F;
}

RcppExport SEXP Rfast_sort_index(SEXP xSEXP,SEXP descendingSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< vector<double> >::type x(xSEXP);
    traits::input_parameter< bool >::type descending(descendingSEXP);
    __result = wrap(sort_index(x,descending));
    return __result;
END_RCPP
}
