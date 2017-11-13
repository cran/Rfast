#include "calc_k_nn_cv.h"

using namespace Rcpp;
using namespace arma;
using namespace std;

// [[Rcpp::export]]
Rcpp::List k_nn_cv(List folds, vec y, mat ds, uvec idxs, const string dist_type, const string type, const string method,
		const unsigned int freq_option, const bool pred_ret, const bool mem_eff) { 
	return calc_k_nn_cv(folds, y, ds, idxs, dist_type, type, method, freq_option, pred_ret, mem_eff);
}


RcppExport SEXP Rfast_k_nn_cv(SEXP foldsSEXP,SEXP ySEXP,SEXP dsSEXP,SEXP idxsSEXP,SEXP dist_typeSEXP,SEXP typeSEXP,SEXP methodSEXP,SEXP freq_optionSEXP,SEXP pred_retSEXP,SEXP mem_effSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< List >::type folds(foldsSEXP);
    traits::input_parameter< vec >::type y(ySEXP);
    traits::input_parameter< mat >::type ds(dsSEXP);
    traits::input_parameter< uvec >::type idxs(idxsSEXP);
    traits::input_parameter< const string >::type dist_type(dist_typeSEXP);
    traits::input_parameter< const string >::type type(typeSEXP);
    traits::input_parameter< const string >::type method(methodSEXP);
    traits::input_parameter< const unsigned int >::type freq_option(freq_optionSEXP);
    traits::input_parameter< const bool >::type pred_ret(pred_retSEXP);
    traits::input_parameter< const bool >::type mem_eff(mem_effSEXP);
    __result = wrap(k_nn_cv(folds,y,ds,idxs,dist_type,type,method,freq_option,pred_ret,mem_eff));
    return __result;
END_RCPP
}
