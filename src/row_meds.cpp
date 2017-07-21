//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
SEXP row_meds(NumericMatrix x){
  const int sz=x.ncol(),p=x.nrow(),middle=sz/2-1;
  int i;
  NumericVector rowi(sz);
  SEXP F=Rf_allocVector(REALSXP,p);
  double *FF=REAL(F);
  if(sz%2==0)
    for(i=0;i<p;++i,++FF){
      rowi=x.row(i);
      nth_element(rowi.begin(),rowi.begin()+middle,rowi.end());
      *FF=(rowi[middle]+*(min_element(rowi.begin()+middle+1,rowi.end())))/2.0;
    }
  else
  	for(i=0;i<p;++i,++FF){
      rowi=x.row(i);
      nth_element(rowi.begin(),rowi.begin()+middle,rowi.end());
      *FF=rowi[middle+1]/2.0;
    }
  return F;
}

// rowMedians
RcppExport SEXP Rfast_row_meds(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = row_meds(x);
    return __result;
END_RCPP
}
