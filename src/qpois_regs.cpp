#include "calc_qpois_regs.h"

#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::vec qpois_regs(arma::mat x, arma::vec y, const double tol, const double ylogy, const double my) {
	return calc_qpois_regs(x, y, tol, ylogy, my);
}


RcppExport SEXP Rfast_qpois_regs(SEXP xSEXP,SEXP ySEXP,SEXP tolSEXP,SEXP ylogySEXP,SEXP mySEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< mat >::type x(xSEXP);
    traits::input_parameter< vec >::type y(ySEXP);
    traits::input_parameter< const double >::type tol(tolSEXP);
    traits::input_parameter< const double >::type ylogy(ylogySEXP);
    traits::input_parameter< const double >::type my(mySEXP);
    __result = wrap(qpois_regs(x,y,tol,ylogy,my));
    return __result;
END_RCPP
}
