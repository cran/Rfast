//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>

using namespace Rcpp;
using std::vector;

vector<int> which_isFactor(DataFrame x){
  vector<int> P;
  DataFrame::iterator xx=x.begin();
  for(int i=1;xx!=x.end();++xx,++i)
    if(Rf_isFactor(*xx))
      P.push_back(i);
  return P;
}

//find which collumns are factors
RcppExport SEXP Rfast_which_isFactor(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< DataFrame >::type x(xSEXP);
    __result = wrap(which_isFactor(x));
    return __result;
END_RCPP
}
