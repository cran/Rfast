#include "calc_bic_fs_reg.h"

using namespace Rcpp;
using std::string;

// [[Rcpp::export]]
NumericMatrix bic_fs_reg(NumericVector y, NumericMatrix ds,const double tol, const string type) {
	return calc_bic_fs_reg(y, ds, tol, type);
}


RcppExport SEXP Rfast_bic_fs_reg(SEXP ySEXP,SEXP dsSEXP,SEXP tolSEXP,SEXP typeSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type y(ySEXP);
    traits::input_parameter< NumericMatrix>::type ds(dsSEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const string >::type type(typeSEXP);
    __result = wrap(bic_fs_reg(y,ds,tol,type));
    return __result;
END_RCPP
}
