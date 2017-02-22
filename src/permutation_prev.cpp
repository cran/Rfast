//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
mat permutation_prev(NumericVector X,const bool all_prev,const int fn){
  unsigned int i=0;
  const int n=X.size();
  mat F(fn,n);
  rowvec x(X.begin(),n,false);
  if(all_prev){
  	do{
  		F.row(i)=x;
  		i++;
  	} while (prev_permutation(x.begin(),x.end()));
  	return F.submat(0,0,i-1,n-1);
  }
  prev_permutation(x.begin(),x.end());
  return x;
}

RcppExport SEXP Rfast_permutation_prev(SEXP xSEXP,SEXP all_prevSEXP,SEXP fnSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< const bool >::type all_prev(all_prevSEXP);
    traits::input_parameter< const int >::type fn(fnSEXP);
    __result = wrap(permutation_prev(x,all_prev,fn));
    return __result;
END_RCPP
}
