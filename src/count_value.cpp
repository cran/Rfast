//Author: Manos Papadakis


#include <RcppArmadillo.h>
#include "templates.h"

using namespace Rcpp;

using std::vector;
using std::string;

int count_value(SEXP x,SEXP value){
  int s=0;
  switch(TYPEOF(value)){
    case REALSXP:
      s=count_value_helper<NumericVector,double>(NumericVector(x),Rf_asReal(value));
      break;
    case INTSXP:
      s=count_value_helper<IntegerVector,int>(IntegerVector(x),Rf_asInteger(value));
      break;
    case STRSXP:
      s=count_value_helper< vector<string>, string >(as< vector<string> >(x),as< string >(value));
      break;
    default:
      stop("Error: Unknown type of argument value.\n");
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


///////////////////////////////////////////////////////////////////


int count_na(SEXP x){
  int s=0;
  switch(TYPEOF(x)){
    case REALSXP:
      s=count_NAs<NumericVector>(NumericVector(x));
      break;
    case INTSXP:
      s=count_NAs<IntegerVector>(IntegerVector(x));
      break;
    default:
      stop("Error: Unknown type of argument x.\n");
      break;
  }
  return s;
}

RcppExport SEXP Rfast_count_na(SEXP x){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    __result = wrap(count_na(x));
    return __result;
END_RCPP
}

////////////////////////////////////////////////////////////////////
