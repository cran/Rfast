//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include "mn.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
SEXP k_comb_n(const int N, const int K){
  SEXP x=Rf_allocVector(INTSXP,K*R::choose(N,K));
  int *xx=INTEGER(x);
  string bitmask(K, 1); // K leading 1's
  bitmask.resize(N, 0); // N-K trailing 0's
  int i;
  do {
    for(i=0;i<N;++i) // [0..N-1] integers
      if(bitmask[i]) 
        *xx++=i+1;
  }while(prev_permutation(bitmask.begin(),bitmask.end()));
  return x;
}

RcppExport SEXP Rfast_k_comb_n(SEXP nSEXP,SEXP kSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< const int >::type n(nSEXP);
    traits::input_parameter< const int >::type k(kSEXP);
    __result = k_comb_n(n,k);
    return __result;
END_RCPP
}

// [[Rcpp::export]]
NumericMatrix find_combn(NumericVector data, const int n) {
	static int combn_col = 0;
	const int size = data.size();
	const int nrows = n;
	const int ncols = R::choose(size, n); 
	NumericMatrix combn_dataset(nrows, ncols);
	std::vector<double> combn_data(n);
	const int start_idx = 0;
	combn_col = 0; 
	combn(data, n, start_idx, combn_data, combn_dataset, combn_col);
	return combn_dataset;
}

RcppExport SEXP Rfast_vec_comb_n(SEXP dataSEXP,SEXP nSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type data(dataSEXP);
    traits::input_parameter< const int >::type n(nSEXP);
    __result = wrap(find_combn(data,n));
    return __result;
END_RCPP
}
