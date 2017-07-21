#include "calc_pc_skel.h"

using namespace arma;
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List pc_skel(mat ds, const string method, const double sig, const unsigned int r) {
	return calc_pc_skel(ds, method, sig, r);
}


RcppExport SEXP Rfast_pc_skel(SEXP dsSEXP,SEXP methodSEXP,SEXP sigSEXP,SEXP rSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< mat >::type ds(dsSEXP);
    traits::input_parameter< const string >::type method(methodSEXP);
    traits::input_parameter< const double >::type sig(sigSEXP);
    traits::input_parameter< const unsigned int >::type r(rSEXP);
    __result = wrap(pc_skel(ds,method,sig,r));
    return __result;
END_RCPP
}
