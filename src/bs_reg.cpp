#include "calc_bs_reg.h"

// [[Rcpp::export]]
Rcpp::List bs_reg(arma::vec y, arma::mat ds, const double sig, const std::string type) {
	return calc_bs_reg(y, ds, sig, type);
}

RcppExport SEXP Rfast_bs_reg(SEXP ySEXP,SEXP dsSEXP,SEXP sigSEXP,SEXP typeSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< arma::vec >::type y(ySEXP);
    traits::input_parameter< arma::mat >::type ds(dsSEXP);
    traits::input_parameter< const double >::type sig(sigSEXP);
    traits::input_parameter< const std::string >::type type(typeSEXP);
    __result = wrap(bs_reg(y,ds,sig,type));
    return __result;
END_RCPP
}
