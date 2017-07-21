//Author: Manos Papadakis

#include <RcppArmadillo.h>
#include <algorithm>
#include <vector>

using namespace Rcpp;
using namespace std;

//[[Rcpp::export]]
List table_string(vector<string> x){
  sort(x.begin(),x.end());
  x.push_back("@");
  vector<string> w;
  vector<int> n;
  vector<string>::iterator a=x.begin(),b=a+1;
  List l;
  int s=1;
  for(;b!=x.end();++b){
    if(*a!=*b){
      n.push_back(s);
      w.push_back(*a);
      a=b;
      s=1;
    }else
      ++s;
  }
  l["freqs"]=n;
  l["values"]=w;
  return l;
}


//[[Rcpp::export]]
List table_double(vector<double> x){
  sort(x.begin(),x.end());
  x.push_back(0);
  vector<double> w;
  vector<int> n;
  vector<double>::iterator a=x.begin(),b=a+1;
  List l;
  int s=1;
  for(;b!=x.end();++b){
    if(*a!=*b){
      n.push_back(s);
      w.push_back(*a);
      a=b;
      s=1;
    }else
      ++s;
  }
  l["freqs"]=n;
  l["values"]=w;
  return l;
}

RcppExport SEXP Rfast_table_string(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< vector<string> >::type x(xSEXP);
    __result = wrap(table_string(x));
    return __result;
END_RCPP
}

RcppExport SEXP Rfast_table_double(SEXP xSEXP){
BEGIN_RCPP
    RObject __result;
    RNGScope __rngScope;
    traits::input_parameter< vector<double> >::type x(xSEXP);
    __result = wrap(table_double(x));
    return __result;
END_RCPP
}
