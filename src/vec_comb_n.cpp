// Author:  Marios Dimitriadis
// Contact: kmdimitriadis@gmail.com

#include <RcppArmadillo.h>
#include "mn.h"

using namespace std;
using namespace Rcpp;

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
