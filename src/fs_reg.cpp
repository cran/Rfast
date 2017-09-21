#include "fs_reg_st.h"
#include "fs_reg_ext.h"

using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
NumericMatrix fs_reg(NumericVector y, NumericMatrix ds,const double sig, const double tol, const string type) {
	if (!type.compare("logistic") || !type.compare("poisson")) {
		return fs_reg_st(y, ds, sig, tol, type);
	}
	else if (!type.compare("quasilogistic") || !type.compare("quasipoisson")) {
		return fs_reg_ext(y, ds, sig, tol, type);
	}
	stop("Unrecognised type.\n");
}


RcppExport SEXP Rfast_fs_reg(SEXP ySEXP,SEXP dsSEXP,SEXP sigSEXP,SEXP tolSEXP,SEXP methodSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type y(ySEXP);
    traits::input_parameter< NumericMatrix>::type ds(dsSEXP);
    traits::input_parameter< const double >::type sig(sigSEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const string >::type method(methodSEXP);
    __result = wrap(fs_reg(y,ds,sig,tol,method));
    return __result;
END_RCPP
}
