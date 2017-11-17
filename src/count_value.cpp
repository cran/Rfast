//Author: Manos Papadakis


#include <RcppArmadillo.h>
#include "templates.h"

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
int count_value(SEXP x,SEXP value){
  int s=0;
  switch(TYPEOF(value)){
    case REALSXP:
      s=count_value_helper<NumericVector,double,double*>(NumericVector(x),Rf_asReal(value));
      break;
    case INTSXP:
      s=count_value_helper<IntegerVector,int,int*>(IntegerVector(x),Rf_asInteger(value));
      break;
    case STRSXP:
      s=count_value_helper< vector<string>, string,vector<string>::iterator >(as< vector<string> >(x),as< string >(value));
      break;
    default:
      stop("Error: Unknown type.\n");
      break;
  }
  return s;
}

RcppExport SEXP Rfast_count_value(SEXP x,SEXP value){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = wrap(count_value(x,value));
    return __result;
END_RCPP
}
