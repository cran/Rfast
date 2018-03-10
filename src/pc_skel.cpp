#include "calc_pc_skel.h"

using namespace Rcpp;
using namespace arma;

using std::string;

Rcpp::List pc_skel(arma::mat ds, const string method, const double sig, const unsigned int r, 
		arma::mat stats_init, arma::mat pvalues_init, arma::ivec is_init_vals) {
	return calc_pc_skel(ds, method, sig, r, stats_init, pvalues_init, is_init_vals);
}

RcppExport SEXP Rfast_pc_skel(SEXP dsSEXP,SEXP methodSEXP,SEXP sigSEXP,SEXP rSEXP,SEXP stats_initSEXP,SEXP pvalues_initSEXP,SEXP is_init_valsSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< mat >::type ds(dsSEXP);
    traits::input_parameter< const string >::type method(methodSEXP);
    traits::input_parameter< const double >::type sig(sigSEXP);
    traits::input_parameter< const unsigned int >::type r(rSEXP);
    traits::input_parameter< mat >::type stats_init(stats_initSEXP);
    traits::input_parameter< mat >::type pvalues_init(pvalues_initSEXP);
    traits::input_parameter< ivec >::type is_init_vals(is_init_valsSEXP);
    __result = wrap(pc_skel(ds,method,sig,r,stats_init,pvalues_init,is_init_vals));
    return __result;
END_RCPP
}
