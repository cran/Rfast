//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include <algorithm>

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
SEXP col_meds(NumericMatrix x){
  const int p=x.ncol(),step=x.nrow(),middle=step/2-1;
  int i;
  NumericVector tmp(step);
  SEXP F=Rf_allocVector(REALSXP,p);
  double *FF=REAL(F);
  if(step%2==0)
    for(i=0;i<p;++i,++FF){
      tmp=x.column(i);
      nth_element(tmp.begin(),tmp.begin()+middle,tmp.end());
      *FF=(tmp[middle]+*(min_element(tmp.begin()+middle+1,tmp.end())))/2.0;
    }
    else 
      for(i=0;i<p;++i,++FF){
        tmp=x.column(i);
        nth_element(tmp.begin(),tmp.begin()+middle+1,tmp.end());
        *FF=tmp[middle+1];
      }
      return F;
}

// colMedians
RcppExport SEXP Rfast_col_meds(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = col_meds(x);
    return __result;
END_RCPP
}
