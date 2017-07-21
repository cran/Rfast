//Author: Manos Papadakis


#include <RcppArmadillo.h>
#include <vector>
#include <string>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
int count_value(NumericVector x,double value){
  int s=0;
  for(NumericVector::iterator a=x.begin();a!=x.end();++a)
    if(*a==value)
      s++;
  return s;
}

//[[Rcpp::export]]
int count_value_string(vector<string> x,string value){
  int s=0;
  for(vector<string>::iterator a=x.begin();a!=x.end();++a)
    if(*a==value)
      s++;
  return s;
}

RcppExport SEXP Rfast_count_value(SEXP xSEXP,SEXP valueSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< NumericVector >::type x(xSEXP);
    traits::input_parameter< double >::type value(valueSEXP);
    __result = wrap(count_value(x,value));
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_count_value_string(SEXP xSEXP,SEXP valueSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< vector<string> >::type x(xSEXP);
    traits::input_parameter< string >::type value(valueSEXP);
    __result = wrap(count_value_string(x,value));
    return __result;
END_RCPP
}
