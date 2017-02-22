//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <vector>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
vector<int> k_comb_n(const int N, const int K){
  vector<int> x;
  string bitmask(K, 1); // K leading 1's
  bitmask.resize(N, 0); // N-K trailing 0's
  int i;
  do {
    for(i=0;i<N;++i) // [0..N-1] integers
      if(bitmask[i]) 
        x.push_back(i+1);
  }while(prev_permutation(bitmask.begin(),bitmask.end()));
  return x;
}

RcppExport SEXP Rfast_k_comb_n(SEXP nSEXP,SEXP kSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const int >::type n(nSEXP);
    traits::input_parameter< const int >::type k(kSEXP);
    __result = wrap(k_comb_n(n,k));
    return __result;
END_RCPP
}
