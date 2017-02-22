//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <R.h>
#include <Rinternals.h>
#include <algorithm>

using namespace Rcpp;
using namespace std;
using namespace arma;

//[[Rcpp::export]]
SEXP colmeds(NumericMatrix x){
  const int p=x.ncol(),step=x.nrow(),middle=step/2-1;
  int i;
  NumericMatrix::iterator first=x.begin(),last=first+step;
  SEXP F=Rf_allocVector(REALSXP,p);
  double *FF=REAL(F);
  if(step%2==0)
    for(i=0;i<p;++i,++FF,first=last,last+=step){
      nth_element(first,first+middle,last);
      *FF=(x(middle,i)+*(min_element(first+middle+1,last)))/2.0;
    }
    else 
      for(i=0;i<p;++i,++FF,first=last,last+=step){
        nth_element(first,first+middle+1,last);
        *FF=x(middle+1,i);
      }
      return F;
}

// colMedians
RcppExport SEXP Rfast_colmeds(SEXP xSEXP) {
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericMatrix >::type x(xSEXP);
    __result = colmeds(x);
    return __result;
END_RCPP
}
