#include <RcppArmadillo.h>

using namespace Rcpp;
using namespace arma;
// [[Rcpp::depends(RcppArmadillo)]]

//[[Rcpp::export]]
List eigs_sym_c(NumericMatrix X,const int k){
  List l;
  mat x(X.begin(),X.nrow(),X.ncol(),false);
  vec eigval;
  mat eigvec;

  eigs_sym( eigval, eigvec, conv_to<sp_mat>::from(x), k);

  l["values"] = flipud(eigval);
  l["vectors"] = fliplr(eigvec);
  return l;
}


RcppExport SEXP Rfast_eigs_sym_c(SEXP XSEXP,SEXP kSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type X(XSEXP);
    traits::input_parameter< const int >::type k(kSEXP);
    __result = eigs_sym_c(X,k);
    return __result;
END_RCPP
}
