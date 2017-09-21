#include "calc_k_nn.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

// [[Rcpp::export]]
arma::mat k_nn(mat ds_extra,vec y,mat ds,uvec idxs,const string dist_type, const string type, const string method,
		const unsigned int freq_option) {
	idxs -= 1;
	return calc_k_nn(ds_extra, y, ds, idxs, dist_type, type, method, freq_option);
}

RcppExport SEXP Rfast_k_nn(SEXP ds_extraSEXP,SEXP ySEXP,SEXP dsSEXP,SEXP idxsSEXP,SEXP dist_typeSEXP,SEXP typeSEXP,SEXP methodSEXP,SEXP freq_optionSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< mat >::type ds_extra(ds_extraSEXP);
    traits::input_parameter< vec >::type y(ySEXP);
    traits::input_parameter< mat >::type ds(dsSEXP);
    traits::input_parameter< uvec >::type idxs(idxsSEXP);
    traits::input_parameter< const string >::type dist_type(dist_typeSEXP);
    traits::input_parameter< const string >::type type(typeSEXP);
    traits::input_parameter< const string >::type method(methodSEXP);
    traits::input_parameter< const unsigned int >::type freq_option(freq_optionSEXP);
    __result = wrap(k_nn(ds_extra,y,ds,idxs,dist_type,type,method,freq_option));
    return __result;
END_RCPP
}
