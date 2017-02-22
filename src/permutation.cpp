//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
mat permutation(NumericVector X,const bool all,const int fn){
  unsigned int i=0;
  const int n=X.size();
  mat F(fn,n);
  rowvec x(X.begin(),n,false);
  sort(x.begin(),x.end());
  if(all){
  	do{
  		F.row(i)=x;
  		i++;
  	} while (next_permutation(x.begin(),x.end()));
  	return F;
  }
  next_permutation(x.begin(),x.end());
  return x;
}

RcppExport SEXP Rfast_permutation(SEXP xSEXP,SEXP allSEXP,SEXP fnSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< const bool >::type all(allSEXP);
    traits::input_parameter< const int >::type fn(fnSEXP);
    __result = wrap(permutation(x,all,fn));
    return __result;
END_RCPP
}
