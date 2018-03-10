#include <RcppArmadillo.h>
#include "calc_perm_cor.h"

using namespace arma;
using namespace Rcpp;

arma::vec perm_cor(arma::vec x, arma::vec y, const unsigned int r) {
	return calc_perm_cor(x, y, r);
}


RcppExport SEXP Rfast_perm_cor(SEXP xSEXP,SEXP ySEXP,SEXP rSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< vec >::type x(xSEXP);
    traits::input_parameter< vec >::type y(ySEXP);
    traits::input_parameter< const unsigned int >::type r(rSEXP);
    __result = wrap(perm_cor(x,y,r));
    return __result;
END_RCPP
}	
